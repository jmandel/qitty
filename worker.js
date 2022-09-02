import init, { q, parse } from "./pkg/qitty.js";

let qopy = () => {};
init().then(() => {
  qopy = (probe, onresult) => {
    const t0 = new Date().getTime();
    const count = q(probe, onresult);
    const elapsedMilliseconds = new Date().getTime() - t0;
    return { count, elapsedMilliseconds };
  };
  postMessage("ready");
});

self.addEventListener("message", async (e) => {
  if (e.data.parse !== undefined) {
    let parsed = parse(e.data.parse.replace(/\s/g, ""));
    self.postMessage({ parsed });
    return;
  }
  else if (e.data.q !== undefined) {
    let q = e.data.q;
    let limit = e.data.limit || Infinity;
    let complete = qopy(q, (result) => {
      self.postMessage({ result });
      return --limit > 0;
    });
    postMessage({ complete });
  }
});
