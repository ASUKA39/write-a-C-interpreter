#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>

#define int long long

// 解释器
int token;              // 当前 token
char *src, *old_src;    // 指向源码字符串的指针
int poolsize;           //
int line;               // 行号

// 虚拟机的虚拟内存
int *text,              // text 段
    *old_text,          //
    *stack;             // 栈区
char *data;             // data 段

// 虚拟机寄存器
int *pc, *bp, *sp, ax, cycle;

// 标识符
int token_val;          // 当前 token 的值
int *current_id,        // 标识数组的窗口数组，用于记录标识符的信息
    *symbols;           // 标识数组，使用数组存放标识符信息
// 标识符信息表：标识符 token，哈希值，标识符名，类型，类别（数字、全局变量等），值，同名时保存全局变量
enum { Token, Hash, Name, Type, Class, Value, BType, BClass, BValue, IdSize};

// 虚拟指令集
enum { LEA, IMM, JMP, CALL,JZ , JNZ, ENT, ADJ, LEV, LI , LC , SI , SC , PUSH,
       OR, XOR, AND, EQ, NE, LT, GT, LE, GE, SHL, SHR, ADD, SUB, MUL, DIV, MOD, 
       OPEN, READ, CLOS, PRTF, MALC, MSET, MCMP, EXIT
};       // 有参数指令在前，无参数指令在后

// token 标记
enum { Num = 128, Fun, Sys, Glo, Loc, Id,
       Char, Else, Enum, If, Int, Return, Sizeof, While,
       Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};
enum { CHAR, INT, PTR};     // 支持的类型
int *idmain;                // main 函数

int basetype;
int expr_type;

int index_of_bp;     // 栈底指针

void next(){            // 用于词法分析，获取下一个标记，它将自动忽略空白字符
    char *last_pos;
    int hash;

    while(token = *src){
        src++;      // 此时 src 移动到下一个位置，token 为上一个 src 的内容

        if(token == '\n'){      // 换行
            line++;
        }
        else if(token == '#'){
            while(*src != 0 && *src != '\n'){   // 不支持宏，直接跳过
                src++;
            }
        }

        // 处理标识符
        else if((token >= 'a' && token <= 'z') || (token >= 'A' && token <= 'Z') || (token == '_')){
            last_pos = src - 1;     // token 当前的位置
            hash = token;
            while((*src >= 'a' && *src <= 'z') || (*src >= 'A' && *src <= 'Z') || (*src >= '0' && *src <= '9') || (*src == '_')){
                hash = hash * 147 + *src;   // 计算哈希值
                src++;
            }
            current_id = symbols;
            // 线性查找此 token 是否已经存在
            while(current_id[Token]){
                if(current_id[Hash] == hash && !memcmp((char *)current_id[Name], last_pos, src - last_pos)){     // 名字匹配且哈希匹配
                    token = current_id[Token];
                    return;
                }
                current_id = current_id + IdSize;   // 移动窗口
            }

            current_id[Name] = (int)last_pos;
            current_id[Hash] = hash;
            token = current_id[Token] = Id;
            return;
        }

        // 处理数字
        else if(token >= '0' && token <= '9'){
            token_val = token - '0';
            if(token_val > 0){
                while(*src >= '0' && *src <= '9'){
                    token_val = token_val * 10 + *src++ - '0';
                }
            }
            else{
                if(*src == 'x' || *src == 'X'){     // 十六进制
                    token = *++src;
                    while((token >= '0' && token <= '9') || (token >= 'a' && token <= 'f') || (token >= 'A' && token <= 'F')){
                        token_val = token_val * 16 + (token & 15) + (token >= 'A' ? 9 : 0);     // token & 15 即获取个位
                        token = *++src;
                    }
                }
                else{                               // 八进制
                    while (*src >= '0' && *src <= '7'){
                        token_val = token_val * 8 + *src++ - '0';
                    }                   
                }
            }
            token = Num;
            return;
        }

        // 处理字符串
        else if(token == '"' || token == '\''){
            last_pos = data;
            while(*src != 0 && *src != token){
                token = *src++;
                if(token_val == '\\'){      // 转义字符
                    token_val = *src++;
                    if(token_val == 'n'){
                        token_val = '\n';
                    }
                }
                if(token == '"'){
                    *data++ = token_val;
                }
            }
            src++;
            if(token == '"'){
                token_val = (int)last_pos;  // 字符串标记其值为一个指针？
            }
            else{
                token = Num;    // 单个字符标记为数字
            }
            return;
        }

        // 处理注释，仅支持 //
        else if(token == '/'){
            if(*src == '/'){    // 查询后一字符是否为 /，是则为注释，否则为除号
                while(*src != 0 && *src != '\n'){
                    ++src;
                }
            }
            else{
                token = Div;
                return;
            }
        }

        // 一些符号
        else if(token == '=') {
            // parse '==' and '='
            if(*src == '=') {
                src ++;
                token = Eq;
            } else {
                token = Assign;
            }
            return;
        }
        else if(token == '+') {
            // parse '+' and '++'
            if(*src == '+') {
                src ++;
                token = Inc;
            } else {
                token = Add;
            }
            return;
        }
        else if(token == '-') {
            // parse '-' and '--'
            if(*src == '-') {
                src ++;
                token = Dec;
            } else {
                token = Sub;
            }
            return;
        }
        else if(token == '!') {
            // parse '!='
            if(*src == '=') {
                src++;
                token = Ne;
            }
            return;
        }
        else if(token == '<') {
            // parse '<=', '<<' or '<'
            if(*src == '=') {
                src ++;
                token = Le;
            } else if(*src == '<') {
                src ++;
                token = Shl;
            } else {
                token = Lt;
            }
            return;
        }
        else if(token == '>') {
            // parse '>=', '>>' or '>'
            if(*src == '=') {
                src ++;
                token = Ge;
            } else if(*src == '>') {
                src ++;
                token = Shr;
            } else {
                token = Gt;
            }
            return;
        }
        else if(token == '|') {
            // parse '|' or '||'
            if(*src == '|') {
                src ++;
                token = Lor;
            } else {
                token = Or;
            }
            return;
        }
        else if(token == '&') {
            // parse '&' and '&&'
            if(*src == '&') {
                src ++;
                token = Lan;
            } else {
                token = And;
            }
            return;
        }
        else if(token == '^') {
            token = Xor;
            return;
        }
        else if(token == '%') {
            token = Mod;
            return;
        }
        else if(token == '*') {
            token = Mul;
            return;
        }
        else if(token == '[') {
            token = Brak;
            return;
        }
        else if(token == '?') {
            token = Cond;
            return;
        }
        else if(token == '~' || token == ';' || token == '{' || token == '}' || token == '(' || token == ')' || token == ']' || token == ',' || token == ':') {
            // directly return the character as token;
            return;
        }
    }
    return;
}

void match(int tk){     // 包装 next()，若非预期标记则报错
    if(token == tk){
        next();
    }
    else{
        printf("%d: expected token: %d\n", line, tk);
        exit(-1);
    }
}

void expression(int level){     // 用于解析一个表达式
    int *id;
    int tmp;
    int *addr;

    if(!token){
        printf("%d: unexpected token EOF of expression\n", line);
        exit(-1);
    }
    else if(token == Num){      // 数字
        match(Num);

        *++text == IMM;
        *++text == token_val;
        expr_type = INT;
    }
    else if(token == '"'){      // 字符串，支持拼接写法
        *++text = IMM;
        *++text = token_val;

        match('"');
        while (token == '"')
        {
            match('"');
        }
        
        data = (char *)(((int)data + sizeof(int)) & (-sizeof(int)));
        expr_type = PTR;
    }
    else if(token == Sizeof){       // 仅支持 int char pointer
        match(Sizeof);
        match('(');
        expr_type = INT;

        if(token == Int){
            match(Int);
        }
        else if(token == Char){
            match(Char);
            expr_type = CHAR;
        }

        while(token == Mul){
            match(Mul);
            expr_type = expr_type + PTR;
        }

        match(')');

        *++text = IMM;
        *++text = (expr_type == CHAR) ? sizeof(char) : sizeof(int);

        expr_type = INT;
    }

    else if(token == Id){
        match(Id);

        id == current_id;

        if(token == '('){       // 调用函数
            match('(');

            tmp = 0;
            while(token != ')'){    // 顺序压栈传参
                expression(Assign);
                *++text = PUSH;
                tmp ++;

                if(token == ','){
                    match(',');
                }
            }
            match(')');

            if(id[Class] == Sys){       // 系统调用型函数直接用指令
                *++text - id[Value];
            }
            else if(id[Class] == Fun){      // 普通函数
                *++text = CALL;
                *++text = id[Value];
            }
            else{
                printf("%d: bad function call\n", line);
                exit(-1);
            }

            if(tmp > 0){        // 改变栈指针清除栈
                *++text = ADJ;
                *++text = tmp;
            }
            expr_type = id[Type];
        }
        else if(id[Class] == Num){
            *++text = IMM;          // 枚举型直接变常数
            *++text = id[Value];
            expr_type = INT;
        }
        else{
            if(id[Class] == Loc){       // 变量值，局部用 bp 相对偏移，全局用 IMM 加载地址
                *++text = LEA;
                *++text = index_of_bp - id[Value];
            }
            else if(id[Class] == Glo){
                *++text = IMM;
                *++text == id[Value];
            }
            else{
                printf("%d: undefined variable\n", line);
                exit(-1);
            }
        }

        expr_type = id[Type];
        *++text = (expr_type == Char) ? LC : LI;    // 加载变量值
    }

    else if(token == '(') {        // 强制类型转换
        // cast or parenthesis
        match('(');
        if(token == Int || token == Char){
            tmp = (token == Char) ? CHAR : INT; // cast type
            match(token);
            while (token == Mul) {
                match(Mul);
                tmp = tmp + PTR;
            }

            match(')');

            expression(Inc); // cast has precedence as Inc(++)

            expr_type  = tmp;
        }
        else{
            // normal parenthesis
            expression(Assign);
            match(')');
        }
    }

    else if(token == Mul){      // 指针取值
        // dereference *<addr>
        match(Mul);
        expression(Inc); // dereference has the same precedence as Inc(++)

        if(expr_type >= PTR){
            expr_type = expr_type - PTR;
        }
        else{
            printf("%d: bad dereference\n", line);
            exit(-1);
        }

        *++text = (expr_type == CHAR) ? LC : LI;
    }

    else if(token == And){      // 取址
        // get the address of
        match(And);
        expression(Inc); // get the address of
        if(*text == LC || *text == LI){
            text --;
        }
        else{
            printf("%d: bad address of\n", line);
            exit(-1);
        }

        expr_type = expr_type + PTR;
    }

    else if(token == '!'){      // 逻辑取反，实际上只需要判断是否等于 0
        // not
        match('!');
        expression(Inc);

        // emit code, use <expr> == 0
        *++text = PUSH;
        *++text = IMM;
        *++text = 0;
        *++text = EQ;

        expr_type = INT;
    }

    else if(token == '~'){      // 按位取反，用异或实现
        // bitwise not
        match('~');
        expression(Inc);

        // emit code, use <expr> XOR -1
        *++text = PUSH;
        *++text = IMM;
        *++text = -1;
        *++text = XOR;

        expr_type = INT;
    }

    else if(token == Add){      // 正号
        // +var, do nothing
        match(Add);
        expression(Inc);

        expr_type = INT;
    }
    else if(token == Sub){      // 负号
        // -var
        match(Sub);

        if(token == Num){
            *++text = IMM;
            *++text = -token_val;
            match(Num);
        }
        else{
            *++text = IMM;
            *++text = -1;
            *++text = PUSH;
            expression(Inc);
            *++text = MUL;
        }

        expr_type = INT;
    }

    else if(token == Inc || token == Dec){      // 自增减
        tmp = token;
        match(token);
        expression(Inc);
        
        if(*text == LC){
            *text = PUSH;
            *++text = LC;
        }
        else if(*text == LI){
            *text = PUSH;
            *++text = LI;
        }
        else{
            printf("%d: bad lvalue of pre-increment\n", line);
            exit(-1);
        }
        *++text = PUSH;
        *++text = IMM;

        // 支持指针自增减
        *++text = (expr_type > PTR) ? sizeof(int) : sizeof(char);
        *++text = (tmp == Inc) ? ADD : SUB;
        *++text = (expr_type == CHAR) ? SC : SI;
    }

    while(token >= level){
        tmp = expr_type;
        if(token == Assign){
            // var = expr;
            match(Assign);
            if(*text == LC || *text == LI){
                *text = PUSH; // save the lvalue's pointer
            }
            else{
                printf("%d: bad lvalue in assignment\n", line);
                exit(-1);
            }
            expression(Assign);

            expr_type = tmp;
            *++text = (expr_type == CHAR) ? SC : SI;
        }

        else if(token == Cond){     // 三目运算符
            // expr ? a : b;
            match(Cond);
            *++text = JZ;
            addr = ++text;
            expression(Assign);
            if(token == ':'){
                match(':');
            }
            else{
                printf("%d: missing colon in conditional\n", line);
                exit(-1);
            }
            *addr = (int)(text + 3);
            *++text = JMP;
            addr = ++text;
            expression(Cond);
            *addr = (int)(text + 1);
        }

        else if(token == Lor){        // 逻辑运算符
            // logic or
            match(Lor);
            *++text = JNZ;
            addr = ++text;
            expression(Lan);
            *addr = (int)(text + 1);
            expr_type = INT;
        }
        else if(token == Lan){
            // logic and
            match(Lan);
            *++text = JZ;
            addr = ++text;
            expression(Or);
            *addr = (int)(text + 1);
            expr_type = INT;
        }

        else if(token == Or){     // 逻辑运算符
            // bitwise or
            match(Or);
            *++text = PUSH;
            expression(Xor);
            *++text = OR;
            expr_type = INT;
        }
        else if(token == Xor){
            // bitwise xor
            match(Xor);
            *++text = PUSH;
            expression(And);
            *++text = XOR;
            expr_type = INT;
        }
        else if(token == And){
            // bitwise and
            match(And);
            *++text = PUSH;
            expression(Eq);
            *++text = AND;
            expr_type = INT;
        }
        else if(token == Eq){
            // equal ==
            match(Eq);
            *++text = PUSH;
            expression(Ne);
            *++text = EQ;
            expr_type = INT;
        }
        else if(token == Ne){
            // not equal !=
            match(Ne);
            *++text = PUSH;
            expression(Lt);
            *++text = NE;
            expr_type = INT;
        }
        else if(token == Lt){
            // less than
            match(Lt);
            *++text = PUSH;
            expression(Shl);
            *++text = LT;
            expr_type = INT;
        }
        else if(token == Gt){
            // greater than
            match(Gt);
            *++text = PUSH;
            expression(Shl);
            *++text = GT;
            expr_type = INT;
        }
        else if(token == Le){
            // less than or equal to
            match(Le);
            *++text = PUSH;
            expression(Shl);
            *++text = LE;
            expr_type = INT;
        }
        else if(token == Ge){
            // greater than or equal to
            match(Ge);
            *++text = PUSH;
            expression(Shl);
            *++text = GE;
            expr_type = INT;
        }
        else if(token == Shl){
            // shift left
            match(Shl);
            *++text = PUSH;
            expression(Add);
            *++text = SHL;
            expr_type = INT;
        }
        else if(token == Shr){
            // shift right
            match(Shr);
            *++text = PUSH;
            expression(Add);
            *++text = SHR;
            expr_type = INT;
        }
        else if(token == Add){
            // add
            match(Add);
            *++text = PUSH;
            expression(Mul);

            expr_type = tmp;
            if(expr_type > PTR){
                // pointer type, and not `char *`
                *++text = PUSH;
                *++text = IMM;
                *++text = sizeof(int);
                *++text = MUL;
            }
            *++text = ADD;
        }
        else if(token == Sub){
            // sub
            match(Sub);
            *++text = PUSH;
            expression(Mul);
            if(tmp > PTR && tmp == expr_type){
                // pointer subtraction
                *++text = SUB;
                *++text = PUSH;
                *++text = IMM;
                *++text = sizeof(int);
                *++text = DIV;
                expr_type = INT;
            }
            else if(tmp > PTR){
                // pointer movement
                *++text = PUSH;
                *++text = IMM;
                *++text = sizeof(int);
                *++text = MUL;
                *++text = SUB;
                expr_type = tmp;
            }
            else{
                // numeral subtraction
                *++text = SUB;
                expr_type = tmp;
            }
        }
        else if(token == Mul){
            // multiply
            match(Mul);
            *++text = PUSH;
            expression(Inc);
            *++text = MUL;
            expr_type = tmp;
        }
        else if(token == Div){
            // divide
            match(Div);
            *++text = PUSH;
            expression(Inc);
            *++text = DIV;
            expr_type = tmp;
        }
        else if(token == Mod){
            // Modulo
            match(Mod);
            *++text = PUSH;
            expression(Inc);
            *++text = MOD;
            expr_type = tmp;
        }

        else if(token == Inc || token == Dec){
            // postfix inc(++) and dec(--)
            // we will increase the value to the variable and decrease it
            // on `ax` to get its original value.
            if(*text == LI){
                *text = PUSH;
                *++text = LI;
            }
            else if(*text == LC){
                *text = PUSH;
                *++text = LC;
            }
            else{
                printf("%d: bad value in increment\n", line);
                exit(-1);
            }

            *++text = PUSH;
            *++text = IMM;
            *++text = (expr_type > PTR) ? sizeof(int) : sizeof(char);
            *++text = (token == Inc) ? ADD : SUB;
            *++text = (expr_type == CHAR) ? SC : SI;
            *++text = PUSH;
            *++text = IMM;
            *++text = (expr_type > PTR) ? sizeof(int) : sizeof(char);
            *++text = (token == Inc) ? SUB : ADD;
            match(token);
        }
        else if(token == Brak){
            // array access var[xx]
            match(Brak);
            *++text = PUSH;
            expression(Assign);
            match(']');

            if(tmp > PTR){
                // pointer, `not char *`
                *++text = PUSH;
                *++text = IMM;
                *++text = sizeof(int);
                *++text = MUL;
            }
            else if(tmp < PTR){
                printf("%d: pointer type expected\n", line);
                exit(-1);
            }
            expr_type = tmp - PTR;
            *++text = ADD;
            *++text = (expr_type == CHAR) ? LC : LI;
        }
        else{
            printf("%d: compiler error, token = %d\n", line, token);
            exit(-1);
        }
    }
}

void statement(){
    // if(...) <statement> [else <statement>]
    // while (...) <statement>
    // { <statement> }
    // return xxx;
    // <empty statement>;
    // expression; (expression end with semicolon)

    int *a, *b; // bess for branch control
    
    if(token == If){
        match(If);
        match('(');
        expression(Assign);
        match(')');

        *++text = JZ;
        b = ++text;

        statement();         // parse statement
        if(token == Else){ // parse else
            match(Else);

            // emit code for JMP B
            *b = (int)(text + 3);
            *++text = JMP;
            b = ++text;

            statement();
        }

        *b = (int)(text + 1);
    }

    else if(token == While){
        match(While);

        a = text + 1;

        match('(');
        expression(Assign);
        match(')');

        *++text = JZ;
        b = ++text;

        statement();

        *++text = JMP;
        *++text = (int)a;
        *b = (int)(text + 1);
    }

    else if(token == Return){
        match(Return);
        
        if(token != ';'){
            expression(Assign);
        }

        match(';');

        *++text = LEV;
    }

    else if(token == '{'){
        // { <statement> ... }
        match('{');

        while (token != '}'){
            statement();
        }

        match('}');
    }
    else if(token == ';'){
        // empty statement
        match(';');
    }
    else{
        // a = b; or function_call();
        expression(Assign);
        match(';');
    }   
}

void enum_declaration(){    // 解析枚举类型，在解析声明时被调用
    int i;
    i = 0;
    while(token != '}'){
        if(token != Id){
            printf("%d: bad enum indentifier %d\n", line, token);
            exit(-1);
        }
        next();
        if(token == Assign){
            next();
            if(token != Num){
                printf("%d: bad enum initializer\n", line);
                exit(-1);
            }
            i = token_val;
            next();
        }

        current_id[Class] = Num;
        current_id[Type] = INT;
        current_id[Value] = i++;

        if(token == ','){
            next();
        }
    }
}

void function_parameter(){      // 函数参数声明解析
    // parameter_decl ::= type {'*'} id {',' type {'*'} id}

    // 解析参数类型
    int type;
    int params;
    params = 0;
    while(token != ')'){
        type = INT;
        if(token == Int){
            match(Int);
        }
        else if(token == Char){
            type = CHAR;
            match(Char);
        }

        while(token == Mul){
            match(Mul);
            type = type + PTR;
        }

        if(token != Id){
            printf("%d: bad parameter declaration\n", line);
            exit(-1);
        }
        if(current_id[Class] == Loc){
            printf("%d: duplicate parameter declaration\n", line);
            exit(-1);
        }
        
        // 将全局变量信息保存到 Bxxx，然后赋为局部变量信息
        current_id[BClass] = current_id[Class]; current_id[Class] = Loc;
        current_id[BType] = current_id[Type];   current_id[Type] = type;
        current_id[BValue] = current_id[Value]; current_id[Value] = params++;   // index of current parameter

        if(token == ',') {
            match(',');
        }   
    }

    // 0 处为 retn_addr，1 处为 bp，保存了旧 bp 的地址
    index_of_bp = params + 1;
}

void function_body(){       // 函数体解析
    // type func_name (...) {...}
    //                   -->|   |<--

    // ... {
    // 1. local declarations
    // 2. statements
    // }

    int pos_local;      // 局部变量在栈上的位置
    int type;
    pos_local = index_of_bp;

    while (token == Int || token == Char) {
        // local variable declaration, just like global ones.
        basetype = (token == Int) ? INT : CHAR;
        match(token);

        while (token != ';') {
            type = basetype;
            while (token == Mul) {
                match(Mul);
                type = type + PTR;
            }

            if(token != Id) {
                // invalid declaration
                printf("%d: bad local declaration\n", line);
                exit(-1);
            }
            if(current_id[Class] == Loc) {
                // identifier exists
                printf("%d: duplicate local declaration\n", line);
                exit(-1);
            }
            match(Id);

            // store the local variable
            current_id[BClass] = current_id[Class]; current_id[Class]  = Loc;
            current_id[BType] = current_id[Type];   current_id[Type] = type;
            current_id[BValue] = current_id[Value]; current_id[Value]  = ++pos_local;   // index of current parameter

            if(token == ',') {
                match(',');
            }
        }
        match(';');
    }

    *++text = ENT;
    *++text = pos_local - index_of_bp;      // 在栈上保存信息

    while(token != '}'){        // 语句解析
        statement();        
    }

    *++text = LEV;
}

void function_declaration(){    // 解析函数声明，在解析声明时被调用
    // type func_name (...) {...}
    //               | this part

    match('(');
    function_parameter();
    match(')');
    match('{');     // 不需要匹配右花括号，因为这被放在声明解析时完成
    function_body();

    // 将同名变量暂时恢复，屏蔽掉同名的全局变量
    current_id = symbols;
    while (current_id[Token]) {
        if(current_id[Class] == Loc) {
            current_id[Class] = current_id[BClass];
            current_id[Type]  = current_id[BType];
            current_id[Value] = current_id[BValue];
        }
        current_id = current_id + IdSize;
    }
}

void global_declaration(){      // 全局声明解析，在 program() 被调用
    // global_declaration ::= enum_decl | variable_decl | function_decl
    //
    // enum_decl ::= 'enum' [id] '{' id ['=' 'num'] {',' id ['=' 'num'} '}'
    //
    // variable_decl ::= type {'*'} id { ',' {'*'} id } ';'
    //
    // function_decl ::= type {'*'} id '(' parameter_decl ')' '{' body_decl '}'

    int type;
    int i;

    basetype = INT;

    if(token == Enum){      // 解析枚举类型
        match(Enum);
        if(token != '{'){
            match(Id);
        }
        if(token == '{'){
            match('{');
            enum_declaration();
            match('}');
        }
        match(';');
        return;
    }

    if(token == Int){      // 解析整数和字符串，到此暂未知晓声明的是变量还是函数
        match(Int);
    }
    else if(token == Char){
        match(Char);
        basetype = CHAR;
    }

    while(token != ';' && token != '}'){
        type = basetype;
        while(token == Mul){    // 指针类型在词法分析时直接定为 mul
            match(Mul);
            type = type + PTR;  // 解析到指针类型则直接 type + ptr 来标记
        }

        if(token != Id){    // 非法声明
            printf("%d: bad global declaration\n", line);
            exit(-1);
        }
        if(current_id[Class]){      // 重复声明
            printf("%d: dulicate global declaration\n", line);
            exit(-1);
        }
        match(Id);
        current_id[Type] = type;

        if(token == '('){       // 解析到括号，说明是在声明函数
            current_id[Class] = Fun;
            current_id[Value] = (int)(text + 1);     // 函数的地址
            function_declaration();
        }
        else{
            current_id[Class] = Glo;    // 否则声明的是全局变量
            current_id[Value] = (int)data;      // 全局变量，data 段地址
            data = data + sizeof(int);
        }

        if(token == ','){
            match(',');
        }
    }
    next();
}

void program(){         // 语法分析的入口，分析整个 C 语言程序
    next();
    while(token > 0){
        global_declaration();
    }
}

int eval(){             // 虚拟机的入口，用于解释目标代码（虚拟机指令）
    int op, *tmp;
    while(1){
        op = *pc++;
        if(op == IMM)       {ax = *pc++;}           // 将参数放入 ax
        else if(op == LC)   {ax = *(char *)ax;}      // 将 ax 地址的字符放入 ax  
        else if(op == LI)   {ax = *(int *)ax;}       // 将 ax 地址的整数放入 ax
        else if(op == SC)   {*(char *)*sp++ = ax;}   // 将 ax 作为字符存入栈顶指向的地址处
        else if(op == SI)   {*(int *)*sp++ = ax;}    // 将 ax 作为整数存入栈顶指向的地址处
        else if(op == PUSH) {*--sp = ax;}           // 出栈
        else if(op == JMP)  {pc = (int *)*pc;}       
        else if(op == JZ)   {pc = ax ? pc + 1 : (int *)*pc;}
        else if(op == JNZ)  {pc = ax ? (int *)*pc : pc + 1;}
        else if(op == CALL) {*--sp = (int)(pc + 1); pc = (int *)*pc;}
        else if(op == ENT)  {*--sp = (int)bp; bp = sp; sp = sp - *pc++;}    // 保存栈基址指针，开辟新栈帧（*pc++ = <size>）
        else if(op == ADJ)  {sp = sp + *pc++;}       // 清除 *pc++ 个栈空间
        else if(op == LEV)  {sp = bp; bp = (int *)*sp++; pc = (int *)*sp++;}  // return from sub-func
        else if(op == LEA)  {ax = (int)(bp + *pc++);}   // 获取参数列表，*pc++ 指向 offset
        else if(op == OR)  ax = *sp++ | ax;
        else if(op == XOR) ax = *sp++ ^ ax;
        else if(op == AND) ax = *sp++ & ax;
        else if(op == EQ)  ax = *sp++ == ax;
        else if(op == NE)  ax = *sp++ != ax;
        else if(op == LT)  ax = *sp++ < ax;
        else if(op == LE)  ax = *sp++ <= ax;
        else if(op == GT)  ax = *sp++ >  ax;
        else if(op == GE)  ax = *sp++ >= ax;
        else if(op == SHL) ax = *sp++ << ax;
        else if(op == SHR) ax = *sp++ >> ax;
        else if(op == ADD) ax = *sp++ + ax;
        else if(op == SUB) ax = *sp++ - ax;
        else if(op == MUL) ax = *sp++ * ax;
        else if(op == DIV) ax = *sp++ / ax;
        else if(op == MOD) ax = *sp++ % ax;
        else if(op == EXIT) { printf("exit %d", *sp); return *sp;}     // 对常用C函数做支持
        else if(op == OPEN) { ax = open((char *)sp[1], sp[0]); }
        else if(op == CLOS) { ax = close(*sp);}
        else if(op == READ) { ax = read(sp[2], (char *)sp[1], *sp);}
        else if(op == PRTF) { tmp = sp + pc[1]; ax = printf((char *)tmp[-1], tmp[-2], tmp[-3], tmp[-4], tmp[-5], tmp[-6]); }
        else if(op == MALC) { ax = (int)malloc(*sp);}
        else if(op == MSET) { ax = (int)memset((char *)sp[2], sp[1], *sp);}
        else if(op == MCMP) { ax = memcmp((char *)sp[2], (char *)sp[1], *sp);}
        else{
            printf("unknown instruction: %d\n", op);
            return -1;
        }
    }
    return 0;
}

int main(int argc, char **argv)
{
    int i, fd;
    int *tmp;

    argc--;
    argv++;

    poolsize = 256 * 1024;
    line = 1;

    if((fd = open(*argv, 0)) < 0){
        printf("could not open file \"%s\"\n", *argv);
        return -1;
    }

    if(!(src = old_src = malloc(poolsize))){
        printf("could not malloc %d for source area\n", poolsize);
        return -1;
    }

    if((i = read(fd, src, poolsize - 1)) <= 0){
        printf("read() return %d\n", i);
        return -1;
    }
    src[i] = 0;     // 给文件加结束符
    close(fd);

    /*-----*/

    if(!(text = old_text = malloc(poolsize))){
        printf("could not malloc %d for text area\n", poolsize);
        return -1;
    }

    if(!(data = malloc(poolsize))){
        printf("could not malloc %d for data area\n", poolsize);
        return -1;
    }

    if(!(stack = malloc(poolsize))){
        printf("could not malloc %d for stack area\n", poolsize);
        return -1;
    }

    memset(text, 0, poolsize);
    memset(data, 0, poolsize);
    memset(stack, 0, poolsize);

    /*-----*/

    bp = sp = (int *)((int)stack + poolsize);    // 初始化栈基址指针和栈顶指针
    ax = 0;                                     // 初始化通用寄存器

    /*-----*/

    src = "char else enum ifint return sizeof while "
          "open read close printf malloc memset memcmp exit void main";
    i = Char;
    while(i <= While){
        next();
        current_id[Token] = i++;
    }
    i = OPEN;
    while(i <= EXIT){
        next();
        current_id[Class] = Sys;
        current_id[Type] = INT;
        current_id[Value] = i++;
    }
    next();
    current_id[Token] = Char;
    next();
    idmain = current_id;

    /*-----*/

    program();

    // setup stack
    sp = (int *)((int)stack + poolsize);
    *--sp = EXIT; // call exit if main returns
    *--sp = PUSH; tmp = sp;
    *--sp = argc;
    *--sp = (int)argv;
    *--sp = (int)tmp;

    return eval();
}