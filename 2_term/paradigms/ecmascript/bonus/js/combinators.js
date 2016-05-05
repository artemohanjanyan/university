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
    return foldLeft(function(z, next) {
        return z.concat([f(next)]);
    }, []);
}

function zip(a, b) {
    var result = [];
    for (var i = 0; i < a.length; ++i) {
        result.push({
            first: a[i],
            second: b[i]
        });
    }
    return result;
}

function andThen(f) {
    return function(g) {
        return function() {
            return g(f.apply(null, arguments));
        }
    }
}

function splitArgs(f) {
    return function(b) {
        return function(a) {
            return f(a, b);
        }
    }
}

function zipWith(f) {
    return andThen(zip)(map(function(x) {
        return f(x.first, x.second);
    }));
}

function add(a, b) {
    return a + b;
}

function mul(a, b) {
    return a * b;
}

var addV = zipWith(add);
var scalar = andThen(zipWith(mul))(foldLeft(add, 0));

var addM = zipWith(addV);

function columns(a) {
    return foldLeft(zipWith(function(column, next) {
        return column.concat([next]);
    }), map(function() { return []; })(a))(a);
}
function mulM(a, b) {
    return map(function(row) {
        //return map(function(column) {
        //    return scalar(row, column);
        //})(columns(b));
        return andThen(columns)(map(splitArgs(scalar)(row)))(b);
    })(a);
}

var square = map(function(a) { return a * a; });

console.log(addM([[1, 2, 3], [4, 5, 6], [7, 8, 9]], [[1, 2, 3], [4, 5, 6], [7, 8, 9]]));
console.log(columns([[1, 2, 3], [4, 5, 6], [7, 8, 9]], [[1, 2, 3], [4, 5, 6], [7, 8, 9]]));
console.log(mulM([[1, 2, 3], [4, 5, 6], [7, 8, 9]], [[1, 2, 3], [4, 5, 6], [7, 8, 9]]));
console.log(mulM([[-1, 2], [2, -1]], [[1/3, 2/3], [2/3, 1/3]]));

console.log(square([1, 2, 3]));
console.log(square([1, 2, 3]));

var A = [
    [
        [
            [-3, -4],
            [2, 2]
        ],
        [
            [-6, -3],
            [4, 0]
        ]
    ],
    [
        [
            [-1, 0],
            [2, 0]
        ],
        [
            [-1, -1],
            [2, 5]
        ]
    ]
];
var T = [
    [-1, 2],
    [2, -1]
];
var S = [
    [1, 2],
    [2, 1]
];

for (var k = 0; k < 2; ++k) {
    for (var l = 0; l < 2; ++l) {
        var str = "";
        for (var i = 0; i < 2; ++i) {
            for (var j = 0; j < 2; ++j) {
                str = str + (A[k][l][i][j] + " * " + S[1][i] + " * " + S[0][j] + " * " + T[k][0] + " * " + T[l][1] + "    ");
            }
        }
        console.log(str);
    }
}
//for (var i = 0; i < 2; ++i) {
//    for (var j = 0; j < 2; ++j) {
//        var str = "";
//        for (var k = 0; k < 2; ++k) {
//            for (var l = 0; l < 2; ++l) {
//                str = str + (A[i][j][k][l] + " * " + S[1][i] + " * " + S[0][j] + " * " + T[k][0] + " * " + T[l][1] + "    ");
//            }
//        }
//        console.log(str);
//    }
//}

var ans = 0;
for (k = 0; k < 2; ++k) {
    for (l = 0; l < 2; ++l) {
        var num = [];
        for (i = 0; i < 2; ++i) {
            for (j = 0; j < 2; ++j) {
                num.push(A[k][l][i][j] * S[1][i] * S[0][j] * T[k][0] * T[l][1]);
            }
        }
        console.log(num);
        ans += foldLeft(add, 0)(num);
    }
}
//for (i = 0; i < 2; ++i) {
//    for (j = 0; j < 2; ++j) {
//        var num = [];
//        for (k = 0; k < 2; ++k) {
//            for (l = 0; l < 2; ++l) {
//                num.push(A[i][j][k][l] * S[1][i] * S[0][j] * T[k][0] * T[l][1]);
//            }
//        }
//        console.log(num);
//        ans += foldLeft(add, 0)(num);
//    }
//}

console.log(ans);