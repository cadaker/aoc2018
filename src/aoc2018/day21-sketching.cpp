int R[6] = {};

void original() {
    // 00 seti 123 0 4
    R[4] = 123;
lbl01:
    // 01 bani 4 456 4
    R[4] &= 456;
    // 02 eqri 4 72 4
    R[4] = R[4] == 72;
    // 03 addr 4 2 2 *
    if (R[4]) goto lbl05;
    // 04 seti 0 0 2 *
    goto lbl01;
lbl05:
    // 05 seti 0 7 4
    R[4] = 0;
lbl06:
    // 06 bori 4 65536 3
    R[3] = R[4] | 65536;
    // 07 seti 10283511 1 4
    R[4] = 10283511;
lbl08:
    // 08 bani 3 255 1
    R[1] = R[3] & 255;
    // 09 addr 4 1 4
    R[4] += R[1];
    // 10 bani 4 16777215 4
    R[4] &= 16777215;
    // 11 muli 4 65899 4
    R[4] *= 65899;
    // 12 bani 4 16777215 4
    R[4] &= 16777215;
    // 13 gtir 256 3 1
    R[1] = 256 > R[3];
    // 14 addr 1 2 2 *
    if (R[1]) goto lbl16;
    // 15 addi 2 1 2 *
    goto lbl17;
lbl16:
    // 16 seti 27 8 2 *
    goto lbl28;
lbl17:
    // 17 seti 0 1 1
    R[1] = 0;
lbl18:
    // 18 addi 1 1 5
    R[5] = R[1] + 1;
    // 19 muli 5 256 5
    R[5] *= 256;
    // 20 gtrr 5 3 5
    R[5] = R[5] > R[3];
    // 21 addr 5 2 2 *
    if (R[5]) goto lbl23;
    // 22 addi 2 1 2 *
    goto lbl24;
lbl23:
    // 23 seti 25 3 2 *
    goto lbl26;
lbl24:
    // 24 addi 1 1 1
    R[1] += 1;
    // 25 seti 17 0 2 *
    goto lbl18;
lbl26:
    // 26 setr 1 4 3
    R[3] = R[1];
    // 27 seti 7 6 2 *
    goto lbl08;
lbl28:
    // 28 eqrr 4 0 1
    R[1] = R[4] == R[0];
    // 29 addr 1 2 2 *
    if (R[1]) return;
    // 30 seti 5 2 2 *
    goto lbl06;
}

void reduced() {
    R[4] = 123;
    do {
    } while((R[4] & 456) != 72);

    R[4] = 0;
    while(true) {
        R[3] = R[4] | 65536;
        R[4] = 10283511;

        while(true) {
            R[4] += R[3] & 0xFF;
            R[4] &= 0xFFFFFF;
            R[4] *= 65899;
            R[4] &= 0xFFFFFF;
            if (R[3] < 256) break;

            //R[1] = 0;
            //while((R[1] + 1) * 256 <= R[3]) {
            //    R[1]++;
            //}
            //R[3] = R[1];
            R[3] = R[3] / 256;
        }

        if (R[4] == R[0]) return;
    }
}

// R[3]   R[1]
// 256 -> 1
// 257 -> 1
// 258 -> 1
// ...
// 511 -> 1
// 512 -> 2
// 513 -> 2
// ...
// 767 -> 2
// 768 -> 3
