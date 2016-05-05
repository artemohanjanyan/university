/**
 * Created by Artem on 21/04/15.
 * Expressions over classes.
 */

function constructor(constructor) {
    return function() {
        var args = arguments;
        function F() {
            constructor.apply(this, args);
        }
        F.prototype = constructor.prototype;
        return new F();
    }
}

var zeroConst = new Const(0);
var oneConst = new Const(1);
var mOneConst = new Const(-1);
function Const(x) {
    this.x = x;
    Object.freeze(this);
}
Const.prototype.evaluate = function() {
    return this.x;
};
Const.prototype.toString = function() {
    return this.x.toString();
};
Const.prototype.prefix = function() {
    return this.x.toString();
};
Const.prototype.diff = function() {
    return zeroConst;
};
Const.prototype.simplify = function() {
    return this;
};

function Variable(name) {
    this.name = name;
    Object.freeze(this);
}
Variable.prototype.toString = function() {
    return this.name;
};
Variable.prototype.prefix = function() {
    return this.name;
};
Variable.prototype.evaluate = function() {
    return arguments[this.name.charCodeAt(0) - "x".charCodeAt(0)];
};
Variable.prototype.diff = function(v) {
    if (this.name == v) {
        return oneConst;
    } else {
        return zeroConst;
    }
};
Variable.prototype.simplify = function() {
    return this;
};

function FunctionalExpression() {
    this.children = arguments;
    this.left = arguments[0];
    this.right = arguments[1];
    Object.freeze(this);
    Object.freeze(this.children);
}
FunctionalExpression.prototype.evaluate = function() {
    var variables = arguments;
    var mappedArgs = Array.prototype.map.call(this.children, function(x) { return x.evaluate.apply(x, variables); });
    return this.f.apply(null, mappedArgs);
};
FunctionalExpression.prototype.toString = function() {
    return Array.prototype.join.call(this.children, " ") + " " + this.fStr;
};
FunctionalExpression.prototype.prefix = function() {
    return "(" + this.fStr + " " + Array.prototype.join.call(
            Array.prototype.map.call(this.children, function(v) {
                return v.prefix();
            }),
            " "
        ) + ")";
};
FunctionalExpression.prototype.simplify = function() {
    var simplifiedChildren = Array.prototype.map.call(this.children, function(x) { return x.simplify(); });
    if (simplifiedChildren.every(function(x) { return x instanceof Const; })) {
        return new Const(this.f.apply(null, simplifiedChildren.map(function(x) { return x.x; })));
    } else {
        var simplified = this.makeSimple.apply(this, simplifiedChildren);
        if (simplified === undefined) {
            return this.constructF.apply(this, simplifiedChildren);
        } else {
            return simplified;
        }
    }
};

function makeFunctional(f, fStr, diff, makeSimple) {
    function Constructor() {
        FunctionalExpression.apply(this, arguments);
    }
    Constructor.prototype = Object.create(FunctionalExpression.prototype);
    Constructor.prototype.constructF = constructor(Constructor);
    Constructor.prototype.f = f;
    Constructor.prototype.fStr = fStr;
    Constructor.prototype.diff = diff;
    if (makeSimple !== undefined) {
        Constructor.prototype.makeSimple = makeSimple;
    } else {
        Constructor.prototype.makeSimple = Constructor.prototype.constructF;
    }
    return Constructor;
}

var Add = makeFunctional(
    function(a, b) {
        return a + b;
    },
    "+",
    function(v) {
        return new Add(this.left.diff(v), this.right.diff(v));
    },
    function(lSimp, rSimp) {
        if (lSimp instanceof Const && lSimp.x == 0) {
            return rSimp;
        }
        if (rSimp instanceof Const && rSimp.x == 0) {
            return lSimp;
        }
    }
);

var Negate = makeFunctional(
    function(x) {
        return -x;
    },
    "negate",
    function(v) {
        return new Negate(this.left.diff(v));
    }
);

var Subtract = makeFunctional(
    function(a, b) {
        return a - b;
    },
    "-",
    function(v) {
        return new Subtract(this.left.diff(v), this.right.diff(v));
    },
    function(lSimp, rSimp) {
        if (lSimp instanceof Const && lSimp.x == 0) {
            return new Multiply(rSimp, mOneConst);
        }
        if (rSimp instanceof Const && rSimp.x == 0) {
            return lSimp;
        }
    }
);

var Multiply = makeFunctional(
    function(a, b) {
        return a * b;
    },
    "*",
    function(v) {
        return new Add(
            new Multiply(this.left.diff(v), this.right),
            new Multiply(this.left, this.right.diff(v))
        );
    },
    function(lSimp, rSimp) {
        if (lSimp instanceof Const) {
            if (lSimp.x == 0) {
                return zeroConst;
            } else if (lSimp.x == 1) {
                return rSimp;
            }
        }
        if (rSimp instanceof Const) {
            if (rSimp.x == 0) {
                return zeroConst;
            } else if (rSimp.x == 1) {
                return lSimp;
            }
        }
    }
);

var Divide = makeFunctional(
    function(a, b) {
        return a / b;
    },
    "/",
    function(v) {
        return new Divide(
            new Subtract(
                new Multiply(this.left.diff(v), this.right),
                new Multiply(this.left, this.right.diff(v))
            ),
            new Multiply(this.right, this.right)
        );
    },
    function(lSimp, rSimp) {
        if (lSimp instanceof Const && lSimp.x == 0) {
            return zeroConst;
        }
        if (rSimp instanceof Const) {
            if (rSimp.x == 1) {
                return lSimp;
            }
        }
    }
);

var Sin = makeFunctional(
    Math.sin,
    "sin",
    function(v) {
        return new Multiply(new Cos(this.left), this.left.diff(v));
    }
);

var Cos = makeFunctional(
    Math.cos,
    "cos",
    function(v) {
        return new Multiply(new Negate(new Sin(this.left)), this.left.diff(v));
    }
);

var ArcTan = makeFunctional(
    Math.atan,
    "atan",
    function(v) {
        var argDiff = this.left.diff(v);
        return new Divide(argDiff, new Add(new Multiply(this.left, this.left), oneConst));
    }
);

var Exp = makeFunctional(
    Math.exp,
    "exp",
    function(v) {
        var argDiff = this.left.diff(v);
        return new Multiply(argDiff, new Exp(this.left));
    }
);

var Parse = function() {
    function isAlpha(c) {
        return 'a' <= c && c <= 'z';
    }
    function isDigit(c) {
        return c.charCodeAt(0) >= 48 && c.charCodeAt(0) < 58;
    }

    var ops = {};
    function addOp(Obj, argN) {
        ops[(new Obj).fStr] = {
            Obj: Obj,
            argN: argN
        }
    }
    addOp(Add, 2);
    addOp(Subtract, 2);
    addOp(Multiply, 2);
    addOp(Divide, 2);
    addOp(Negate, 1);
    addOp(Sin, 1);
    addOp(Cos, 1);
    addOp(ArcTan, 1);
    addOp(Exp, 1);

    var whitespaces = /\s+/;

    return {
        parse: function(string) {
            return string.split(whitespaces).reduce(function(stack, token) {
                if (token != "") {
                    if (token in ops) {
                        var args = stack.slice(-ops[token].argN);
                        stack.length -= ops[token].argN;
                        stack.push(constructor(ops[token].Obj).apply(this, args));
                    } else if (isAlpha(token[0])) {
                        stack.push(new Variable(token));
                    } else {
                        stack.push(new Const(parseInt(token)));
                    }
                }
                return stack;
            }, [])[0];
        },

        parsePrefix: function() {
            var i = 0;
            var str;

            function tokenToStr(c) {
                if (c === undefined) {
                    return "end of expression";
                } else {
                    return "'" + c + "'";
                }
            }

            function UnexpectedInputError(expected, found) {
                this.name = "UnexpectedInputError";
                this.message = expected + " expected, but " + tokenToStr(found) + " found at char No " + (i + 1);
            }
            UnexpectedInputError.prototype = Object.create(Error.prototype);

            function UnknownIdError(found) {
                this.name = "UnknownIdError";
                this.message = "unknown identifier " + tokenToStr(found) + " found at char No " + (i + 1);
            }
            UnknownIdError.prototype = Object.create(Error.prototype);

            function skipWhile(predicate) {
                while (i < str.length && predicate(str[i])) {
                    ++i;
                }
            }
            function takeWhile(predicate) {
                var i1 = i;
                skipWhile(predicate);
                return str.slice(i1, i);
            }
            function isWhitespace(c) {
                return c.match(whitespaces);
            }
            function skipWhitespaces() {
                skipWhile(isWhitespace);
            }

            function go() {
                skipWhitespaces();
                if (i == str.length) {
                    throw new UnexpectedInputError("expression", str[i]);
                }
                if (str[i] == '(') {
                    ++i;
                    skipWhitespaces();
                    var fName = takeWhile(function(c) {
                        return c != '(' && c != ')' && !isWhitespace(c);
                    });
                    if (!(fName in ops)) {
                        throw new UnexpectedInputError("function", fName);
                    }

                    var args = [];
                    for (var i1 = 0; i1 < ops[fName].argN; ++i1) {
                        args.push(go());
                    }
                    skipWhitespaces();
                    if (str[i] != ')') {
                        throw new UnexpectedInputError(')', str[i]);
                    }
                    ++i;

                    return constructor(ops[fName].Obj).apply(null, args);
                } else if (isAlpha(str[i])) {
                    var name = takeWhile(isAlpha);
                    if (name != 'x' && name != 'y' && name != 'z') {
                        throw new UnknownIdError(name);
                    }
                    return new Variable(name);
                } else if (isDigit(str[i]) || str[i] == "-") {
                    var numStr = str[i++];
                    numStr += takeWhile(isDigit);
                    var number = parseInt(numStr);
                    if (isNaN(number)) {
                        throw new UnexpectedInputError("number", numStr);
                    }
                    return new Const(number);
                } else {
                    throw new UnexpectedInputError("expression", str[i]);
                }
            }

            return function(string) {
                i = 0;
                str = string;
                var ans = go();
                skipWhitespaces();
                if (i < str.length) {
                    throw new UnexpectedInputError("end of expression", str[i]);
                }
                return ans;
            }
        }()
    };
}();

var parse = Parse.parse;
var parsePrefix = Parse.parsePrefix;

//var q = new ArcTan(new Subtract(new Variable('x'), new Variable('y'))).diff('x');
//console.log(q.toString());
//console.log(q.simplify().prefix());