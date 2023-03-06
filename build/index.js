const app = Elm.Main.init({
  node: document.getElementById("app"),
});

document.addEventListener("keydown", (ev) => {
  const deleteKeyNames = ["backspace", "delete"];
  const deleteKeyCodes = [8];
  if (ev.keyCode >= 65 && ev.keyCode <= 90) {
    app.ports.receiveCharacter.send(ev?.key?.toLowerCase());
  }
  if (deleteKeyNames.includes(ev.key) || deleteKeyCodes.includes(ev.keyCode)) {
    app.ports.receiveBackspace.send(null);
  }
});
