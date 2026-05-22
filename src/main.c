#include <ctype.h>
#include <linux/limits.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>
#include <sys/stat.h>
#include <dirent.h>

typedef enum {
    PatternTypeChar,
    PatternTypeDigit,
    PatternTypeWord,
    PatternTypeGroup,
    PatternTypeGroupReverse,
    PatternTypeQuantifier,
    PatternTypeWildcard,
    PatternTypeAlternation,
    PatternTypeBackReference,
    PatternTypeStart,
    PatternTypeEnd,
    PatternTypeMax,
} PatternType;

typedef struct Pattern Pattern;

typedef struct {
    Pattern *chain;
    size_t chain_size;
} Branch;

struct Pattern {
    PatternType type;
    PatternType basetype;
    union {
       char ch;
       char *group;
       struct {
           Branch *branches;
           int branch_count;
       } alt;
    } v;
    int min_times;
    int max_times;
    int group_num;
    const char *match_start;
    const char *match_end;
};

typedef struct {
    bool only_matching;
    bool use_color;
    bool recursive;
    const char *filename;
    bool *matched;
} GrepOpts;

const char *capture_group_start[10];
const char *capture_group_end[10];
int capture_group_count;

static void free_chain(Pattern *chain, size_t chain_size);
Pattern *parse_pattern_chain(const char *pattern, size_t *chain_size);

static void free_branches(Branch *branches, int count) {
    for (int i = 0; i < count; i++) {
        free_chain(branches[i].chain, branches[i].chain_size);
    }
    free(branches);
}

static bool contains(const char *group, char c) {
    while (*group) {
        if (c == *group++) {
            return true;
        }
    }
    return false;
}

static int split_branches(const char *content, size_t len, char ***out_branches) {
    int cap = 4, count = 0;
    char **branches = malloc(cap * sizeof(char*));
    const char *bs = content;
    for (size_t i = 0; i <= len; i++) {
        if (i < len && content[i] == '\\') { i++; continue; }
        if (i < len && content[i] == '[') {
            i++;
            if (i < len && content[i] == '^') i++;
            while (i < len && content[i] != ']') { if (content[i] == '\\') i++; i++; }
            continue;
        }
        if (i < len && content[i] == '(') {
            int d = 1; i++;
            while (i < len && d > 0) { if (content[i] == '(') d++; else if (content[i] == ')') d--; i++; }
            i--;
            continue;
        }
        if (content[i] == '|' || i == len) {
            int blen = content + i - bs;
            if (count + 1 > cap) { cap *= 2; branches = realloc(branches, cap * sizeof(char*)); }
            branches[count] = calloc(1, blen + 1);
            memcpy(branches[count], bs, blen);
            count++;
            bs = content + i + 1;
        }
    }
    *out_branches = branches;
    return count;
}

Pattern *parse_pattern_chain(const char *pattern, size_t *chain_size) {
    size_t chain_cap = 4;
    size_t chain_size_t = 0;
    Pattern *chain = calloc(chain_cap, sizeof(*chain));
    if (*pattern == '^') {
        chain[chain_size_t].type = PatternTypeStart;
        chain[chain_size_t].basetype = PatternTypeStart;
        chain_size_t++;
        pattern++;
    }
    while (*pattern) {
        if (chain_size_t + 1 > chain_cap) {
            chain_cap *= 2;
            chain = realloc(chain, chain_cap * sizeof(*chain));
        }
        if (strncmp(pattern, "\\d", 2) == 0) {
            pattern += 2;
            chain[chain_size_t].basetype = PatternTypeDigit;
            chain[chain_size_t++].type = PatternTypeDigit;
        } else if (strncmp(pattern, "\\w", 2) == 0) {
            pattern += 2;
            chain[chain_size_t].basetype = PatternTypeWord;
            chain[chain_size_t++].type = PatternTypeWord;
        } else if (*pattern == '$' && pattern[1] == '\0') {
            chain[chain_size_t].basetype = PatternTypeEnd;
            chain[chain_size_t++].type = PatternTypeEnd;
            pattern++;
        } else if (pattern[0] == '[') {
            ++pattern;
            bool reverse = false;
            if (*pattern == '^') {
                reverse = true;
                pattern++;
            }
            const char *group_s = pattern;
            size_t group_len = 0;
            while (*pattern && *pattern != ']') {
                pattern++;
            }
            if (*pattern == ']') {
                group_len = pattern - group_s;
                char *group = calloc(1, group_len + 1);
                memcpy(group, group_s, group_len);
                chain[chain_size_t].type = reverse ? PatternTypeGroupReverse : PatternTypeGroup;
                chain[chain_size_t].basetype = chain[chain_size_t].type;
                chain[chain_size_t++].v.group = group;
                pattern++;
            }
        } else if (*pattern == '+') {
            if (chain_size_t > 0) {
                chain[chain_size_t - 1].type = PatternTypeQuantifier;
                chain[chain_size_t - 1].min_times = 1;
                chain[chain_size_t - 1].max_times = -1;
            }
            pattern++;
        } else if (*pattern == '?') {
            if (chain_size_t > 0) {
                chain[chain_size_t - 1].type = PatternTypeQuantifier;
                chain[chain_size_t - 1].min_times = 0;
                chain[chain_size_t - 1].max_times = 1;
            }
            pattern++;
        } else if (*pattern == '.') {
            chain[chain_size_t].type = PatternTypeWildcard;
            chain[chain_size_t].basetype = chain[chain_size_t].type;
            chain_size_t++;
            pattern++;
        } else if (*pattern == '*') {
            if (chain_size_t > 0) {
                chain[chain_size_t - 1].type = PatternTypeQuantifier;
                chain[chain_size_t - 1].min_times = 0;
                chain[chain_size_t - 1].max_times = -1;
            }
            pattern++;
        } else if (*pattern == '(') {
            ++pattern;
            const char *alter_s = pattern;
            int depth = 1;
            while (*pattern && depth > 0) {
                if (*pattern == '\\') { pattern++; if (*pattern) pattern++; continue; }
                if (*pattern == '(') depth++;
                else if (*pattern == ')') depth--;
                if (depth > 0) pattern++;
            }
            if (*pattern == ')') {
                size_t alter_len = pattern - alter_s;
                int my_group = ++capture_group_count;
                char **branch_strs;
                int branch_count = split_branches(alter_s, alter_len, &branch_strs);
                Branch *branches = malloc(branch_count * sizeof(Branch));
                for (int b = 0; b < branch_count; b++) {
                    size_t sub_size;
                    branches[b].chain = parse_pattern_chain(branch_strs[b], &sub_size);
                    branches[b].chain_size = sub_size;
                    free(branch_strs[b]);
                }
                free(branch_strs);
                chain[chain_size_t].type = chain[chain_size_t].basetype = PatternTypeAlternation;
                chain[chain_size_t].group_num = my_group;
                chain[chain_size_t].v.alt.branches = branches;
                chain[chain_size_t].v.alt.branch_count = branch_count;
                chain_size_t++;
            }
            pattern++;
        } else if (*pattern == '{') {
            ++pattern;
            const char *times_s = pattern;
            while (*pattern && *pattern != '}') pattern++;
            if (*pattern == '}' && chain_size_t > 0) {
                size_t len = pattern - times_s;
                char *buf = calloc(1, len + 1);
                memcpy(buf, times_s, len);
                char *comma = strchr(buf, ',');
                if (comma) {
                    *comma = '\0';
                    chain[chain_size_t - 1].min_times = atoi(buf);
                    chain[chain_size_t - 1].max_times = *(comma + 1) ? atoi(comma + 1) : -1;
                } else {
                    chain[chain_size_t - 1].min_times = atoi(buf);
                    chain[chain_size_t - 1].max_times = atoi(buf);
                }
                chain[chain_size_t - 1].type = PatternTypeQuantifier;
                free(buf);
            }
            pattern++;
        } else if (*pattern == '\\') {
            pattern++;
            if (isdigit(*(pattern))) {
                chain[chain_size_t].type = PatternTypeBackReference;
                chain[chain_size_t].basetype = chain[chain_size_t].type;
                chain[chain_size_t++].v.ch = *pattern;
                pattern++;
            }
        } else {
            chain[chain_size_t].type = PatternTypeChar;
            chain[chain_size_t].basetype = chain[chain_size_t].type;
            chain[chain_size_t++].v.ch = *pattern;
            pattern++;
        }
    }
    *chain_size = chain_size_t;
    return chain;
}

static void fill_chain_matched(Pattern *chain, size_t chain_size, bool *matched, const char *ori) {
    if (matched == NULL) {
        return;
    }
    for (int i = 0; i < chain_size; i++) {
        if (chain[i].match_start != NULL) {
            for (int j = 0; j < chain[i].match_end - chain[i].match_start + 1; j++) {
                matched[chain[i].match_start + j - ori] = true;
            }
        }
    }
}

static void printf_match_chain(Pattern *chain, size_t chain_size) {
    for (int i = 0; i < chain_size; i++) {
        if (chain[i].match_start != NULL) {
            for (int j = 0; j < chain[i].match_end - chain[i].match_start + 1; j++) {
                putchar(chain[i].match_start[j]);
            }
        }
    }
    putchar('\n');
}

static void free_chain(Pattern *chain, size_t chain_size) {
    for (int i = 0; i < chain_size; i++) {
        if (chain[i].basetype == PatternTypeGroup || chain[i].basetype == PatternTypeGroupReverse) {
            free(chain[i].v.group);
        } else if (chain[i].basetype == PatternTypeAlternation) {
            free_branches(chain[i].v.alt.branches, chain[i].v.alt.branch_count);
        }
    }
    free(chain);
}

bool match_chain_start(const char *input_line, const char *line_start, Pattern *chain, size_t chain_size);

bool match_chain_base_type(char ch, Pattern *chain) {
    bool res = false;
    switch (chain->basetype) {
        case PatternTypeChar:
            res = (chain->v.ch == ch);
            break;
        case PatternTypeDigit:
            res = isdigit(ch);
            break;
        case PatternTypeGroup:
            res = contains(chain->v.group, ch);
            break;
        case PatternTypeGroupReverse:
            res = !contains(chain->v.group, ch);
            break;
        case PatternTypeWord:
            res = isalnum(ch) || ch == '_';
            break;
        case PatternTypeWildcard:
            res = true;
            break;
        default:
            break;
    }
    return res;
}

const char *match_one_unit(const char *input, Pattern *p) {
    if (p->basetype == PatternTypeAlternation) {
        int saved_cg = capture_group_count;
        capture_group_count = p->group_num;
        for (int b = 0; b < p->v.alt.branch_count; b++) {
            Branch *br = &p->v.alt.branches[b];
            if (match_chain_start(input, input, br->chain, br->chain_size)) {
                const char *end = input;
                for (int j = 0; j < br->chain_size; j++) {
                    if (br->chain[j].match_start) p->match_start = br->chain[j].match_start;
                    if (br->chain[j].match_end && br->chain[j].match_end >= end)
                        end = br->chain[j].match_end + 1;
                }
                p->match_end = end - 1;
                if (p->group_num > 0) {
                    capture_group_start[p->group_num] = p->match_start;
                    capture_group_end[p->group_num] = p->match_end;
                }
                capture_group_count = saved_cg;
                return end;
            }
        }
        capture_group_count = saved_cg;
        return NULL;
    }
    if (!*input || !match_chain_base_type(*input, p)) return NULL;
    return input + 1;
}

bool match_chain_start(const char *input_line, const char *line_start, Pattern *chain, size_t chain_size) {
    if (chain_size == 0) return true;
    chain->match_start = NULL;

    switch (chain->type) {
        case PatternTypeStart:
            if (input_line != line_start) return false;
            return match_chain_start(input_line, line_start, chain + 1, chain_size - 1);

        case PatternTypeEnd:
            return *input_line == '\0';

        case PatternTypeQuantifier: {
            const char *positions[256];
            int count = 0;
            const char *p = input_line;
            int max = (chain->max_times < 0) ? 255 : chain->max_times;
            for (int i = 0; i < max; i++) {
                const char *next = match_one_unit(p, chain);
                if (!next) break;
                positions[count++] = p;
                p = next;
            }
            if (count < chain->min_times) return false;
            for (int i = count; i >= chain->min_times; i--) {
                if (i > 0) {
                    chain->match_start = input_line;
                    chain->match_end = p - 1;
                } else {
                    chain->match_start = NULL;
                }
                if (match_chain_start(p, line_start, chain + 1, chain_size - 1)) return true;
                if (i > 0) p = positions[i - 1];
            }
            return false;
        }

        case PatternTypeAlternation: {
            int saved_cg = capture_group_count;
            capture_group_count = chain->group_num;
            size_t rest = chain_size - 1;
            for (int b = 0; b < chain->v.alt.branch_count; b++) {
                Branch *br = &chain->v.alt.branches[b];
                const char *saved_cap_start = capture_group_start[chain->group_num];
                const char *saved_cap_end = capture_group_end[chain->group_num];

                if (match_chain_start(input_line, line_start, br->chain, br->chain_size)) {
                    const char *branch_end = input_line;
                    const char *branch_start = NULL;
                    for (int j = 0; j < br->chain_size; j++) {
                        if (br->chain[j].match_start && !branch_start)
                            branch_start = br->chain[j].match_start;
                        if (br->chain[j].match_end && br->chain[j].match_end + 1 > branch_end)
                            branch_end = br->chain[j].match_end + 1;
                    }
                    if (chain->group_num > 0) {
                        capture_group_start[chain->group_num] = branch_start;
                        capture_group_end[chain->group_num] = branch_end - 1;
                    }

                    size_t total = br->chain_size + rest;
                    Pattern *comb = malloc(total * sizeof(Pattern));
                    memcpy(comb, br->chain, br->chain_size * sizeof(Pattern));
                    memcpy(comb + br->chain_size, chain + 1, rest * sizeof(Pattern));
                    bool ok = match_chain_start(input_line, line_start, comb, total);
                    if (ok) {
                        const char *end = input_line;
                        chain->match_start = NULL;
                        for (int j = 0; j < br->chain_size; j++) {
                            if (comb[j].match_start && !chain->match_start)
                                chain->match_start = comb[j].match_start;
                            if (comb[j].match_end && comb[j].match_end >= end)
                                end = comb[j].match_end + 1;
                        }
                        chain->match_end = end - 1;
                        if (chain->group_num > 0) {
                            capture_group_start[chain->group_num] = chain->match_start;
                            capture_group_end[chain->group_num] = chain->match_end;
                        }
                        memcpy(chain + 1, comb + br->chain_size, rest * sizeof(Pattern));
                        free(comb);
                        capture_group_count = saved_cg;
                        return true;
                    }
                    free(comb);
                    capture_group_start[chain->group_num] = saved_cap_start;
                    capture_group_end[chain->group_num] = saved_cap_end;
                }
            }
            capture_group_count = saved_cg;
            return false;
        }

        case PatternTypeBackReference: {
            int n = chain->v.ch - '0';
            if (!capture_group_start[n]) return false;
            int len = capture_group_end[n] - capture_group_start[n] + 1;
            if (memcmp(input_line, capture_group_start[n], len) != 0) return false;
            chain->match_start = input_line;
            chain->match_end = input_line + len - 1;
            return match_chain_start(input_line + len, line_start, chain + 1, chain_size - 1);
        }

        default:
            if (!*input_line) return false;
            if (!match_chain_base_type(*input_line, chain)) return false;
            chain->match_start = chain->match_end = input_line;
            return match_chain_start(input_line + 1, line_start, chain + 1, chain_size - 1);
    }
}

bool match_chain(const char *input_line, Pattern *chain, size_t chain_size, GrepOpts *opts) {
    int input_len = strlen(input_line);
    bool res = false;
    for (int i = 0; i <= input_len;) {
        if (match_chain_start(input_line + i, input_line, chain, chain_size)) {
            if (opts->only_matching) {
                printf_match_chain(chain, chain_size);
            }
            fill_chain_matched(chain, chain_size, opts->matched, input_line);
            const char *max_end = input_line + i;
            for (int j = 0; j < chain_size; j++) {
                if (chain[j].match_end && chain[j].match_end >= max_end) {
                    max_end = chain[j].match_end;
                }
            }
            i += max_end - (input_line + i) + 1;
            res = true;
        } else {
            i++;
        }
    }
    return res;
}

static void print_matched_with_color(bool *matched, const char *input_line) {
    int i = 0;
    while (*input_line != '\0') {
        if (matched[i] && (i == 0 || !matched[i - 1])) printf("\033[1;31m");
        if (!matched[i] && i > 0 && matched[i - 1]) printf("\033[0m");
        putchar(*input_line);
        input_line++;
        i++;
    }
    if (i > 0 && matched[i - 1]) printf("\033[0m");
    putchar('\n');
}

bool match_pattern(const char* input_line, const char* pattern, GrepOpts *opts) {
    size_t chain_size;
    capture_group_count = 0;
    memset(capture_group_start, 0, sizeof(capture_group_start));
    memset(capture_group_end, 0, sizeof(capture_group_end));
    if (opts->use_color) {
        opts->matched = calloc(1, strlen(input_line) * sizeof(*opts->matched));
    }
    Pattern *chain = parse_pattern_chain(pattern, &chain_size);
    bool res = match_chain(input_line, chain, chain_size, opts);
    if (res) {
        const char *prefix = opts->filename ? opts->filename : "";
        const char *sep = opts->filename ? ":" : "";
        if (opts->only_matching) {
        } else if (opts->matched != NULL) {
            printf("%s%s", prefix, sep);
            print_matched_with_color(opts->matched, input_line);
        } else {
            printf("%s%s%s\n", prefix, sep, input_line);
        }
    }
    free(opts->matched);
    opts->matched = NULL;
    free_chain(chain, chain_size);
    return res;
}

static int grep_stream(FILE *fp, const char *pattern, GrepOpts *opts) {
    int res = 1;
    char input_line[1024];
    capture_group_count = 0;
    while (fgets(input_line, sizeof(input_line), fp)) {
        input_line[strcspn(input_line, "\n")] = '\0';
        if (match_pattern(input_line, pattern, opts)) {
            res = 0;
        }
    }
    return res;
}

static bool is_dir(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) return false;
    return S_ISDIR(st.st_mode);
}

static int grep_path(const char *path, const char *pattern, GrepOpts *opts) {
    if (is_dir(path)) {
        int res = 1;
        DIR *dir = opendir(path);
        if (!dir) return 1;
        struct dirent *entry;
        char child[PATH_MAX];
        size_t dirlen = strlen(path);
        if (dirlen > 0 && path[dirlen - 1] == '/') dirlen--;
        while ((entry = readdir(dir)) != NULL) {
            if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) continue;
            snprintf(child, sizeof(child), "%.*s/%s", (int)dirlen, path, entry->d_name);
            if (grep_path(child, pattern, opts) == 0) res = 0;
        }
        closedir(dir);
        return res;
    }
    FILE *fp = fopen(path, "r");
    if (!fp) return 1;
    opts->filename = path;
    int res = grep_stream(fp, pattern, opts);
    fclose(fp);
    return res;
}

static struct option long_options[] = {
  {"color", required_argument, NULL, 'c'},
  {0, 0, 0, 0}  
};

int main(int argc, char* argv[]) {
    setbuf(stdout, NULL);
    setbuf(stderr, NULL);
    fprintf(stderr, "Logs from your program will appear here\n");

    int opt;
    GrepOpts opts = {0};
    const char *pattern;
    int res = 1;
    const char *color = "never";

    while ((opt = getopt_long(argc, argv, "orE:", long_options, NULL)) != -1) {
        switch (opt) {
            case 'o': opts.only_matching = true; break;
            case 'E': pattern = optarg; break;
            case 'c': color = optarg; break;
            case 'r': opts.recursive = true; break;
            default: return 1;
        }
    }

    if (strcmp(color, "always") == 0) {
        opts.use_color = true;
    } else if (strcmp(color, "auto") == 0) {
        opts.use_color = isatty(STDOUT_FILENO);
    }

    int npaths = argc - optind;
    if (npaths == 0) {
        opts.filename = NULL;
        res = grep_stream(stdin, pattern, &opts);
    } else {
        for (int i = 0; i < npaths; i++) {
            const char *path = argv[optind + i];
            if (opts.recursive) {
                if (grep_path(path, pattern, &opts) == 0) res = 0;
            } else {
                opts.filename = npaths > 1 ? path : NULL;
                FILE *fp = fopen(path, "r");
                if (fp) {
                    if (grep_stream(fp, pattern, &opts) == 0) res = 0;
                    fclose(fp);
                }
            }
        }
    }
    return res;
}
