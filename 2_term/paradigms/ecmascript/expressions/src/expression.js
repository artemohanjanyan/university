function foldLeft(f, zero) {
    return function(a) {
        var z = zero;
        for (var i = 0; i < a.length; ++i) {
            z = f(z, a[i]);
        }
        return z;
    }
}

function map(f) {
    return function(array) {
        var mapped = [];
        for (var i = 0; i < array.length; ++i) {
            mapped.push(f(array[i]));
        }
        return mapped;
    }
}

function cnst(a) {
    return function() {
        return a;
    }
}

function variable(name) {
    return function(x, y, z, holder) {
        if (name == "x" || name == "y" || name == "z") {
            return arguments[name.charCodeAt(0) - "x".charCodeAt(0)];
        } else {
            return holder[name];
        }
    }
}

function functionExpr(operation) {
    return function() {
        var children = arguments;
        return function() {
            var evaluateArgs = arguments;
            var args = map(function(f) {
                return f.apply(null, evaluateArgs);
            })(children);
            return operation.apply(null, args);
        }
    }
}

var add = functionExpr(function(a, b) {
    return a + b;
});
var subtract = functionExpr(function(a, b) {
    return a - b;
});
var multiply = functionExpr(function(a, b) {
    return a * b;
});
var divide = functionExpr(function(a, b) {
    return a / b;
});
var mod = functionExpr(function(a, b) {
    return a % b;
});
var power = functionExpr(Math.pow);

var negate = functionExpr(function(a) {
    return -a;
});
var abs = functionExpr(Math.abs);
var log = functionExpr(Math.log);

var min = functionExpr(Math.min);
var max = functionExpr(Math.max);

function parse(string) {
    function isAlpha(c) {
        return 'a' <= c && c <= 'z';
    }

    function makeOp(op, argN) {
        return {
            op: op,
            argN: argN
        }
    }
    var ops = {};
    ops["+"] = makeOp(add, 2);
    ops["-"] = makeOp(subtract, 2);
    ops["*"] = makeOp(multiply, 2);
    ops["/"] = makeOp(divide, 2);
    ops["%"] = makeOp(mod, 2);
    ops["**"] = makeOp(power, 2);
    ops["negate"] = makeOp(negate, 1);
    ops["abs"] = makeOp(abs, 1);
    ops["log"] = makeOp(log, 1);

    return foldLeft(function(stack, token) {
        if (token.search("min") == 0 || token.search("max") == 0) {
            var f;
            if (token.search("min") == 0) {
                f = min;
            } else {
                f = max;
            }
            var num = parseInt(token.slice(3, token.length));
            var args = stack.slice(-num);
            stack.length -= num;
            stack.push(f.apply(null, args));
        } else if (token in ops) {
            args = stack.slice(-ops[token].argN);
            stack.length -= ops[token].argN;
            stack.push(ops[token].op.apply(null, args));
        } else if (isAlpha(token[0])) {
            stack.push(variable(token));
        } else {
            stack.push(cnst(parseInt(token)));
        }
        return stack;
    }, [])(string.split(/\s+/))[0];
}

function testExpr(str) {
    console.log('"' + str + '"');
    var expr = parse(str);
    for (var i = -10; i <= 10; ++i) {
        console.log(i + ":    " + expr(i, 0, 0));
    }
    console.log();
}

//testExpr("x x * 2 x * - 1 +");

//console.log(parse("x q ** ")(3, 0, 0, {
//    "q": 3
//}));

testExpr("x y z min3");
console.log(min(variable("x"), cnst(1), cnst(2))(-1));