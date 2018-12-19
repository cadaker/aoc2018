int A[6] = {0, 0, 0, 0, 0, 0};

void original() {
    // 0 addi 5 16 5 *
    goto lbl17;
lbl1:
    // 1 seti 1 0 4
    A[4] = 1;
lbl2:
    // 2 seti 1 8 1
    A[1] = 1;
lbl3:
    // 3 mulr 4 1 3
    A[3] = A[4]*A[1];
    // 4 eqrr 3 2 3
    A[3] = A[3] == A[2];
    // 5 addr 3 5 5 *
    if(A[3]) goto lbl7;
    // 6 addi 5 1 5 *
    goto lbl8;
lbl7:
    // 7 addr 4 0 0
    A[0] += A[4];
lbl8:
    // 8 addi 1 1 1
    A[1]++;
    // 9 gtrr 1 2 3
    A[3] = A[1] > A[2];
    //10 addr 5 3 5 *
    if (A[3]) goto lbl12;
    //11 seti 2 4 5 *
    goto lbl3;
lbl12:
    //12 addi 4 1 4
    A[4]++;
    //13 gtrr 4 2 3
    A[3] = A[4] > A[2];
    //14 addr 3 5 5 *
    if(A[3]) goto lbl16;
    //15 seti 1 7 5 *
    goto lbl2;
lbl16:
    //16 mulr 5 5 5 *
    return;
lbl17:
    //17 addi 2 2 2
    A[2] += 2;
    //18 mulr 2 2 2
    A[2] *= A[2];
    //19 mulr 5 2 2
    A[2] *= 19;
    //20 muli 2 11 2
    A[2] *= 11;
    //21 addi 3 6 3
    A[3] += 6;
    //22 mulr 3 5 3
    A[3] *= 22;
    //23 addi 3 9 3
    A[3] += 9;
    //24 addr 2 3 2
    A[2] += A[3];
    //25 addr 5 0 5 *
    if(A[0]) goto lbl27;
    //26 seti 0 5 5 *
    goto lbl1;
lbl27:
    //27 setr 5 9 3
    A[3] = 27;
    //28 mulr 3 5 3
    A[3] *= 28;
    //29 addr 5 3 3
    A[3] += 29;
    //30 mulr 5 3 3
    A[3] *= 30;
    //31 muli 3 14 3
    A[3] *= 14;
    //32 mulr 3 5 3
    A[3] *= 32;
    //33 addr 2 3 2
    A[2] += A[3];
    //34 seti 0 1 0
    A[0] = 0;
    //35 seti 0 0 5 *
    goto lbl1;
}

void simplified() {
    //17 addi 2 2 2
    //A[2] += 2;
    //18 mulr 2 2 2
    //A[2] *= A[2];
    //19 mulr 5 2 2
    //A[2] *= 19;
    //20 muli 2 11 2
    //A[2] *= 11;
    //21 addi 3 6 3
    //A[3] += 6;
    //22 mulr 3 5 3
    //A[3] *= 22;
    //23 addi 3 9 3
    //A[3] += 9;
    //24 addr 2 3 2
    //A[2] += A[3];
    A[2] = 4*19*11 + 6*22 + 9;
    //25 addr 5 0 5 *
    if(A[0]) {
        //27 setr 5 9 3
        //A[3] = 27;
        //28 mulr 3 5 3
        //A[3] *= 28;
        //29 addr 5 3 3
        //A[3] += 29;
        //30 mulr 5 3 3
        //A[3] *= 30;
        //31 muli 3 14 3
        //A[3] *= 14;
        //32 mulr 3 5 3
        //A[3] *= 32;
        //33 addr 2 3 2
        A[2] += (27*28 + 29) * 30 * 14 * 32;
        //34 seti 0 1 0
        A[0] = 0;
    }
    A[4] = 1;
    do {
        if (A[2] % A[4] == 0) {
            A[0] += A[4];
        }
        //A[1] = 1;
        //do {
        //    if(A[4]*A[1] == A[2]) {
        //        A[0] += A[4];
        //    }
        //    A[1]++;
        //} while (A[1] <= A[2]);
        A[4]++;
    } while(A[4] <= A[2]);
}
