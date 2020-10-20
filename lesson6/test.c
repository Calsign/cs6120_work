#include <stdio.h>

// this is a required proxy for printf
// return 0 because I didn't figure out how to return null in the LLVM API
int print_msg(char *msg) {
    printf("%s\n", msg);
    return 0;
}

int get_x() {
    return 3;
}

void recurse(int x) {
    if (x > 0) {
        recurse(x - 1);
    }
}

int main() {
    recurse(10);
    int x = 32 + get_x();
    printf("%i\n", x);
}
