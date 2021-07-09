let map = fn(arr, f) {
    let iter = fn(arr, acc) {
        if (len(arr) == 0) {
            acc
        } else {
            iter(tail(arr), push(acc, f(head(arr))));
        }
    };
    iter(arr, []);
};

let a = [1, 2, 3, 4];
map(a, fn(x) { x * 2 });
