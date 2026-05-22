#include <ctype.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>

typedef enum {
    PatternTypeChar,
    PatternTypeDigit,
    PatternTypeWord,
    PatternTypeGroup,
    PatternTypeGroupReverse,
    PatternTypeOneMoreTime,
    PatternTypeZeroOrOne,
    PatternTypeWildcard,
    PatternTypeAlternation,
    PatternTypeStart,
    PatternTypeEnd,
    PatternTypeMax,
} PatternType;

typedef struct {
    PatternType type;
    PatternType basetype;
    union {
       char ch;
       char *group;
    } v;
    const char *match_start;
    const char *match_end;
} Pattern;

typedef struct {
    bool only_matching;
    bool use_color;
    const char *filename;
    bool *matched;
} GrepOpts;

static bool contains(const char *group, char c) {
    while (*group) {
        if (c == *group++) {
            return true;
        }
    }
    return false;   
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
                chain[chain_size_t - 1].type = PatternTypeOneMoreTime;
                pattern++;
            }
        } else if (*pattern == '?') {
            if (chain_size_t > 0) {
                chain[chain_size_t - 1].type = PatternTypeZeroOrOne;
                pattern++;
            }           
        } else if (*pattern == '.') {
            chain[chain_size_t].type = PatternTypeWildcard;
            chain[chain_size_t].basetype = chain[chain_size_t].type;
            chain_size_t++;
            pattern++;
        } else if (*pattern == '(') {
            ++pattern;
            const char *alter_s = pattern;
            size_t alter_len = 0;
            while (*pattern && *pattern != ')') {
                pattern++;
            }
            if (*pattern == ')') {
                alter_len = pattern - alter_s;
                char *group = calloc(1, alter_len + 1);
                memcpy(group, alter_s, alter_len);
                chain[chain_size_t].type = PatternTypeAlternation;
                chain[chain_size_t].basetype = chain[chain_size_t].type;
                chain[chain_size_t++].v.group = group;
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
        if (chain[i].basetype == PatternTypeGroup || chain[i].basetype == PatternTypeGroupReverse || chain[i].basetype == PatternTypeAlternation) {
            free(chain[i].v.group);
        }
    }
    free(chain);
}

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

const char *match_alternation(const char *input_line, char *alternation) {
    char *prev_s = alternation;
    int alter_len = strlen(alternation);
    for (int i = 0; i < alter_len; i++) {
        if (alternation[i] == '|') {
            int len = alternation + i - prev_s;
            if (memcmp(input_line, prev_s, len) == 0) {
                return input_line + len;
            }
            prev_s = alternation + i + 1;
        }
    }
    int len = alternation + alter_len - prev_s;
    if (memcmp(input_line, prev_s, len) == 0) {
        return input_line + len;
    }
    return NULL;
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

        case PatternTypeZeroOrOne:
            if (*input_line && match_chain_base_type(*input_line, chain)) {
                chain->match_start = chain->match_end = input_line;
                return match_chain_start(input_line + 1, line_start, chain + 1, chain_size - 1);
            }
            if (match_chain_start(input_line, line_start, chain + 1, chain_size - 1)) {
                return true;
            }
            return false;

        case PatternTypeOneMoreTime: {
            if (!*input_line || !match_chain_base_type(*input_line, chain)) return false;
            const char *p = input_line + 1;
            while (*p && match_chain_base_type(*p, chain)) p++;
            chain->match_start = input_line;
            for (; p >= input_line + 1; p--) {
                chain->match_end = p - 1;
                if (match_chain_start(p, line_start, chain + 1, chain_size - 1)) return true;
            }
            return false;
        }

        case PatternTypeAlternation: {
            const char *next = match_alternation(input_line, chain->v.group);
            if (next == NULL) {
                return false;
            }
            chain->match_start = input_line;
            chain->match_end = next - 1;
            return match_chain_start(next, line_start, chain + 1, chain_size - 1);
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
    if (opts->use_color) {
        opts->matched = calloc(1, strlen(input_line) * sizeof(*opts->matched));
    }
    Pattern *chain = parse_pattern_chain(pattern, &chain_size);
    bool res = match_chain(input_line, chain, chain_size, opts);
    if (res) {
        const char *prefix = opts->filename ? opts->filename : "";
        const char *sep = opts->filename ? ":" : "";
        if (opts->only_matching) {
            // already printed in match_chain
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

static struct option long_options[] = {
  {"color", required_argument, NULL, 'c'},
  {0, 0, 0, 0}  
};

int main(int argc, char* argv[]) {
    // Disable output buffering
    setbuf(stdout, NULL);
    setbuf(stderr, NULL);

    // You can use print statements as follows for debugging, they'll be visible when running tests.
    fprintf(stderr, "Logs from your program will appear here\n");

    int opt;
    GrepOpts opts = {0};
    const char* pattern;
    int res = 1;
    const char *color = "never";
    FILE *fp_s[1] = {stdin};
    FILE **fp = fp_s;
    int fp_num = 1;

    while ((opt = getopt_long(argc, argv, "oE:", long_options, NULL)) != -1) {
        switch (opt) {
            case 'o':
                opts.only_matching = true;
                break;
            case 'E':
                pattern = optarg;
                break;
            case 'c':
                color = optarg;
                break;
            default:
                return 1;
        }
    }

    if (strcmp(color, "always") == 0) {
        opts.use_color = true;
    } else if (strcmp(color, "auto") == 0) {
        opts.use_color = isatty(STDOUT_FILENO);
    }

    if (optind < argc) {
        fp_num = argc - optind;
        fp = malloc(fp_num * sizeof(*fp));
        for (int i = 0; i < fp_num; i++) {
            fp[i] = fopen(argv[optind + i], "r");
        }
    }

    char input_line[1024];
    for (int i = 0; i < fp_num; i++) {
        opts.filename = (fp_num > 1 && optind < argc) ? argv[optind + i] : NULL;
        while (fgets(input_line, sizeof(input_line), fp[i]) != NULL) {
            // Remove trailing newline
            input_line[strcspn(input_line, "\n")] = '\0';
            if (match_pattern(input_line, pattern, &opts)) {
                res = 0;
            }
        }

    }
    if (fp != fp_s) {
        free(fp);
    }
    return res;
}
