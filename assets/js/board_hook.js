let BoardHook = {
  updated() {
    // obtain data from the phx hook component
    let lastRow = this.el.dataset.lastRow;
    let lastCol = this.el.dataset.lastCol;
    let curPlayerNum = this.el.dataset.currentPlayerNumber;
    this.playerClass = this.el.dataset.playerClass;

    this.circle = document.getElementById("circle");
    circle.setAttribute("class", "circle p" + curPlayerNum);

    let pos = document.getElementById(lastRow.toString() + lastCol.toString());
    console.log(pos);
    if (curPlayerNum == "") {
      this.removeElementsByClass("chip");
    } else {
      this.addChip(pos)
    }
  },
  addChip(pos){
    if (pos != null) {
      let chip = this.circle.cloneNode(true);
      chip.setAttribute("class", "circle chip " + this.playerClass);
  
      pos.appendChild(chip);
    }
  },
  removeElementsByClass(className){
    var elements = document.getElementsByClassName(className);
    while(elements.length > 0){
        elements[0].parentNode.removeChild(elements[0]);
    }
  }
};

export { BoardHook };
