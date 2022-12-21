import * as readline from "readline";

export const emitKeypressEvents = () =>
    readline.emitKeypressEvents(process.stdin);

export const getKey = (cb) => () => {

    const fn = (str, key) => {
        if (key.ctrl && key.name === "c") {
            process.exit();
        } else {
            process.stdin.setRawMode(false);
            cb(key)();
        }
    }

    process.stdin.setRawMode(true);
    process.stdin.resume();

    process.stdin.removeAllListeners("keypress");
    process.stdin.once("keypress", fn);
};