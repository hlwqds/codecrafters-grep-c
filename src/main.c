#include <ctype.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


typedef enum {
    PatternTypeChar,
    PatternTypeDigit,
    PatternTypeWord,
    PatternTypeGroup,
    PatternTypeGroupReverse,
    PatternTypeOneMoreTime,
    PatternTypeZeroOrOne,
    PatternTypeWildcard,
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
} Pattern;

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
            pattern++;
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

static void free_chain(Pattern *chain, size_t chain_size) {
    for (int i = 0; i < chain_size; i++) {
        if (chain[i].basetype == PatternTypeGroup || chain[i].basetype == PatternTypeGroupReverse) {
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

bool match_chain_start(const char *input_line, Pattern *chain, size_t chain_size) {
    if (chain_size == 0) {
        return true;
    }
    if (chain->type == PatternTypeEnd) {
        return *input_line == '\0';
    }
    if (!*input_line) {
        return false;
    }
    bool res = false;
    switch (chain->type) {
        case PatternTypeOneMoreTime:
            res = match_chain_base_type(*input_line++, chain);
            if (!res) {
                break;
            }
            const char *start_t = input_line;
            while (*input_line) {
                res = match_chain_base_type(*input_line++, chain);
                if (!res) {
                    break;
                }
            }
            while (input_line >= start_t) {
                res = match_chain_start(input_line--, chain + 1, chain_size - 1);
                if (res) {
                    return res;
                }
            }
            break;
        case PatternTypeZeroOrOne:
            res = match_chain_start(input_line, chain + 1, chain_size - 1);
            if (res) {
                return res;
            }
            res = match_chain_base_type(*input_line, chain);
            break;
        default:
            res = match_chain_base_type(*input_line, chain);
            break;
    }
    if (!res) {
        return res;
    }
    return match_chain_start(input_line + 1, chain + 1, chain_size - 1);
}

bool match_chain(const char *input_line, Pattern *chain, size_t chain_size) {
    int input_len = strlen(input_line);
    for (int i = 0; i <= input_len; i++) {
        if (match_chain_start(input_line + i, chain, chain_size)) {
            return true;
        }
    }
    return false;
}

bool match_pattern(const char* input_line, const char* pattern) {
    size_t chain_size;
    bool anchor = false;
    if (*pattern == '^') {
        anchor = true;
        pattern++;
    }
    Pattern *chain = parse_pattern_chain(pattern, &chain_size);
    bool res;
    if (anchor) {
        res = match_chain_start(input_line, chain, chain_size);
    } else {
        res = match_chain(input_line, chain, chain_size);
    }
    free_chain(chain, chain_size);
    return res;
}

int main(int argc, char* argv[]) {
    // Disable output buffering
    setbuf(stdout, NULL);
    setbuf(stderr, NULL);

    // You can use print statements as follows for debugging, they'll be visible when running tests.
    fprintf(stderr, "Logs from your program will appear here\n");

    if (argc != 3) {
        fprintf(stderr, "Expected two arguments\n");
        return 1;
    }

    const char* flag = argv[1];
    const char* pattern = argv[2];

    if (strcmp(flag, "-E") != 0) {
        fprintf(stderr, "Expected first argument to be '-E'\n");
        return 1;
    }

    char input_line[1024];
    if (fgets(input_line, sizeof(input_line), stdin) == NULL) {
        return 1;
    }
    
    // Remove trailing newline
    input_line[strcspn(input_line, "\n")] = '\0';
    
    if (match_pattern(input_line, pattern)) {
        return 0;
    } else {
        return 1;
    }
}
