import init, { q } from "./pkg/qitty.js";

let qopy = () => {};
init().then(() => {
  qopy = (probe, onresult) => {
    const t0 = new Date().getTime();
    const count = q(probe, onresult);
    const elapsedMilliseconds = new Date().getTime() - t0;
    return { count, elapsedMilliseconds, };
  };

  q("start"); // force evaluation of lazy_static DICTIONARY
  postMessage("ready");
});

self.addEventListener("message", async (e) => {
  let q = e.data.q;
  let limit = e.data.limit || Infinity;
  let complete = qopy(q, (result)=> {
    self.postMessage({result})
    return --limit > 0
  });
  postMessage({ complete });
});
