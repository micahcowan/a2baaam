#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define STREQ   !strcmp
#define ENTRIES_PER_LINE    8

const char *progName;

int main(int argc, char **argv) {
    int i, m;
    size_t z;
    const char *fileNames[2];
    FILE *files[2];
    size_t skipBytes = 0;

    /* Save our invocation name */
    progName = *argv ? *argv : "mkfixup";
    ++argv;

    /* First, check for -s (skip) option. */
    if (*argv != NULL && STREQ(*argv, "-s")) {
        ++argv;
        if (*argv == NULL) {
            fprintf(stderr, "%s: -s must be followed by a number.\n", progName);
            return 2;
        }
        skipBytes = strtoul(*argv, NULL, 0);
        ++argv;
    }

    /* Get the files. */
    for (i=0; i!=2; ++i) {
        if (*argv == NULL) {
            fprintf(stderr, "%s: Insufficient number of args.\n", progName);
            fprintf(stderr, "Usage: %s [ -s SKIPBYTES ] file1 file2\n",
                    progName);
            return 2;
        }
        fileNames[i] = *argv;
        errno = 0;
        files[i] = fopen(*argv, "rb");
        if (files[i] == NULL) {
            fprintf(stderr, "%s: Could not open file %s: %s\n",
                    progName, *argv, strerror(errno));
            return 1;
        }
        ++argv;
    }

    if (*argv != NULL) {
        fprintf(stderr, "%s: Extra args given.\n", progName);
        fprintf(stderr, "Usage: %s [ -s SKIPBYTES ] file1 file2\n",
                progName);
        return 2;
    }

    /* Skip leading bytes, do not count toward byte count. */
    if (skipBytes) {
        fprintf(stderr, "%s: Skipping %lu bytes...\n", progName,
               (unsigned long)skipBytes);
        for (z=0; z!=skipBytes; ++z) {
            for (i=0; i!=2; ++i) {
                int c = getc(files[i]);
                if (c == EOF) {
                    fprintf(stderr, "%s: File %s ended while skipping "
                            "byte %lu.\n", progName, fileNames[i],
                            (unsigned long)z);
                    return 1;
                }
            }
        }
    }
    /* Now compare the files. */
    z = 0;
    m = 0;
    for (;;) /* EVER */ {
        int c = getc(files[0]);
        int d = getc(files[1]);
        if (c == EOF || d == EOF) {
            if ((c == EOF) != (d == EOF)) {
                putchar('\n');
                fprintf(stderr, "%s: *** Files are not the same length!\n",
                        progName);
                return 1;
            }
            else {
                /* Both files ended; we're done. */
                fputs("\n.word  $0000\n", stdout);
                return 0;
            }
        }

        if (c != d) {
            /* Print the location where they differed */
            if (m == 0) {
                fputs(".word  ", stdout);
            }
            else {
                fputs(", ", stdout);
            }

            printf("$%04X", (unsigned int)z);

            ++m;
            if (m == ENTRIES_PER_LINE) {
                m = 0;
                putchar('\n');
            }
        }

        ++z;
    }
}
