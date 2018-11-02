const tolerance = 0.000001;

const fixedPoint = (f, firstGuess) => {
  const tryIt = guess => {
    const next = f(guess);
    return closeEnough(next, guess) ? next : tryIt(next);
  }
  const closeEnough = (a, b) => Math.abs(a - b) < tolerance;
  return tryIt(firstGuess);
}

const compose = (f, g) => x => f(g(x));

const repeat = (f, times) =>
  times === 0 ?
    x => x :
    compose(f, repeat(f, times - 1));

const averageDump = f => x => ((x + f(x)) / 2);

const fixedPointOfTransform = (f, transform, guess) =>
  fixedPoint(transform(f), guess);

const even = x => ((x % 2) === 0);

const power = (base, n) => {
  let a = 0;
  const iter = (x, times, result) => 
    times === 0 ?
      result :
      (times % 2) === 0 ?
        iter(x*x, times/2, result) :
        iter(x, times-1, result*x);
  return iter(base, n, 1);
}

const timesOfAverageDump = n => {
  const iter = times =>
    power(2, times) > n ?
      times - 1 :
      iter(times + 1);
  return iter(0);
}

const nthRoots = (n, x) => {
  const f = y => (x / power(y, n-1));
  return fixedPointOfTransform(
    f,
    repeat(averageDump, timesOfAverageDump(n)),
    1.0
  );
}

const a = 149;
console.log(nthRoots(a, power(2, a)));
