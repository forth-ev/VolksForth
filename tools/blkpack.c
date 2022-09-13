#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>

static int lineno;

static void emptylines(int n)
{
  for (int i=0; i<64*n; i++) putchar(0x20);
}

static int getmarker(char *line) // returns -1 on error, blkid otherwise
{
    int blkid;
    int r = sscanf(line, "( ----- %d )\n", &blkid);
    if (r == 1) {
        return blkid;
    } else {
        return -1;
    }
}

static int expectmarker(char *line)
{
    int blkid = getmarker(line);
    if (blkid < 0) { // could not scan
        fprintf(
            stderr, "Error at line %d: expecting block marker\n", lineno);
    }
    return blkid;
}

static void usage()
{
    fprintf(stderr, "Usage: blkpack < blk.fs > blkfs\n");
}

int main(int argc, char *argv[])
{
    int prevblkid = -1;
    int blkid;
    char *line = NULL;
    if (argc != 1) {
        usage();
        return 1;
    }
    lineno = 1;
    size_t n = 0;
    ssize_t cnt = getline(&line, &n, stdin);
    if (cnt <= 0) {
        fprintf(stderr, "No input\n");
        return 1;
    }
    while (1) {
        blkid = expectmarker(line);
        if (blkid < 0) return 1;
        if (blkid <= prevblkid) {
            fprintf(
                stderr,
                "Wrong blkid (%d) at line %d: blocks must be ordered\n",
                blkid, lineno);
            return 1;
        }
        emptylines((blkid-prevblkid-1)*16);
        int blkline;
        for (blkline=0; blkline<16; blkline++) {
            lineno++;
            cnt = getline(&line, &n, stdin);
            if (cnt <= 0) break; // EOF
            if (cnt > 65) {
                fprintf(stderr, "Line %d too long (blk %d)\n", lineno, blkid);
                return 1;
            }
            if (getmarker(line) >= 0) break; // we have a marker early
            line[cnt-1] = '\0'; // remove newline
            printf("%s", line);
            // pad line to 64 chars
            for (int i=cnt-1; i<64; i++) putchar(0x20);
        }
        if (blkline == 16) {
            lineno++;
            cnt = getline(&line, &n, stdin);
        } else {
            // fill to 16 lines
            emptylines(16-blkline);
        }
        if (cnt <= 0) break; // EOF
        prevblkid = blkid;
    }
    free(line);
    return 0;
}

