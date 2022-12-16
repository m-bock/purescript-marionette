import * as readline from "readline";


export const emitKeypressEvents = () =>
    readline.emitKeypressEvents(process.stdin);

export const getKeyImpl = () =>
    new Promise((res) => {
        process.stdin.setRawMode(true);

        process.stdin.once("keypress", (str, key) => {
            if (key.ctrl && key.name === "c") {
                process.exit();
            } else {
                process.stdin.setRawMode(false);
                res(key);
            }
        });
    });