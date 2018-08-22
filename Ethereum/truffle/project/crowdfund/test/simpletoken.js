var SimpleToken = artifacts.require("./SimpleToken.sol");

contract(SimpleToken, function(accounts) {
    
    var OwnerTokenBalance;

    it("check init", function() {
        
        return SimpleToken.deployed().then(function(instance) {
            return instance.balanceOf.call(accounts[0]);
        }).then(function(bal) {
            assert.isTrue(!!bal, "Init nubmer should be there");

            console.log("Init owner balance is ", bal.toNumber());
            
            assert.equal(bal.toNumber(), 1000000, "Init nubmer should be 1000000");

            OwnerTokenBalance = bal.toNumber();

            return web3.eth.getBalance(SimpleToken.address);
        }).then(function(bal) {
            console.log("ether balance of accounts[0] is ", bal.toNumber());   
        });
        
    });

    it("check transfer", function() {
        var ins;
        var FromAccount = accounts[0];
        var ToAccount = accounts[1];
        var ToAmount = 20;

        return SimpleToken.deployed().then(function(instance) {

            ins = instance;

            return ins.transferTo(ToAccount, ToAmount, {from: FromAccount});

        }).then(function() {

            return ins.balanceOf.call(ToAccount);
            
        }).then(function(bal) {
            console.log("Get transfered amount:", bal.toNumber());
            assert.equal(bal.toNumber(), ToAmount, "To-account should receive amount.");
            return ins.balanceOf.call(FromAccount);
        }).then(function(bal) {
            console.log("From-account decresed to ", bal.toNumber());
            assert.equal(bal.toNumber(), OwnerTokenBalance - ToAmount, "From-account should decrese amount.");
            OwnerTokenBalance -= ToAmount;
        });
    });

});
