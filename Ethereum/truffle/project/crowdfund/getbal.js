function checkBalance() {
    var totalBal = 0;
    for (var acntNum in web3.eth.accounts) {
        var act = web3.eth.accounts[acntNum];
        var actBal = web3.fromWei(web3.eth.getBalance(act), "ether");
        totalBal += parseFloat(actBal);
        console.log(" web3.eth.accounts[" + acntNum + "]: \t" + act + " \tbalance: " + actBal + " ether");
    }
    console.log(" Total balance: " + totalBal + " ether");
};
checkBalance()
