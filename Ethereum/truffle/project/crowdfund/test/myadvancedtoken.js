var MyAdvancedToken = artifacts.require("./MyAdvancedToken.sol");
var Crowdsale = artifacts.require("./Crowdsale.sol");

function sleep(time) {
    return new Promise((resolve) => setTimeout(resolve, time));
}

contract("MyAdvancedToken", function (accounts) {

    const expInitialSupply = 100000;
    const expTokenName = "MyAdCoin";
    const expTokenSymbol = "MACr";

    const deployAccount = accounts[0];
    const auxiAccount = accounts[1];
    const gasAccount = accounts[8];
    const tokenOwnerAccount = deployAccount;

    const ETHER = 1e18;

    // MACr per Wei
    const SELLPRICE = 1e15;
    const BUYPRICE  = 1e15;

    const CONTRACT_CHARGE_VALUE = ETHER;

    const APPROVE_LIMIT = 1000000;

    var tokenBalOfOwner;
    var tokenBalOfContract;
    var tokenBalOfGas;
    var tokenBalOfAux;
    var tokenTotalSupply;

    var contractWeiBal;

    it("test charge operation", function() {
            // Charge contract from account[5] to support sell()
        return MyAdvancedToken.deployed().then(function(instance) {
            web3.eth.sendTransaction({
                from: accounts[5],
                to: MyAdvancedToken.address,
                value: CONTRACT_CHARGE_VALUE  // 1 ether, 10^18 Wei
            });

        }).then(function() {

            return web3.eth.getBalance(MyAdvancedToken.address);

        }).then(function(bal) {

            assert.equal(1, !!bal, "Wei balance of contract address should be there");
            // WARNING: deviation under 100 won't be noticed
            contractWeiBal = bal.toNumber();
            assert.equal(contractWeiBal, CONTRACT_CHARGE_VALUE, "Wei balance of contract address should match value charged");

        });
    });

    it("check token init supply", function() {
        return MyAdvancedToken.deployed().then(function(instance) {
            return instance.totalSupply.call();
        }).then(function(totalSupply) {
            assert.equal(totalSupply.valueOf(), expInitialSupply * 10**2, "init total supply should match init-supply!");
        });
    });

    it("check token name", function() {
        return MyAdvancedToken.deployed().then(function(instance) {
            return instance.name.call();
        }).then(function(name) {
            assert.equal(name.valueOf(), expTokenName, "token name set wrong!");
        });
    });

    it("check token symbol", function() {
        return MyAdvancedToken.deployed().then(function(instance) {
            return instance.symbol.call();
        }).then(function(symbol) {
            assert.equal(symbol.valueOf(), expTokenSymbol, "token symbol set wrong!");
        });
    });

    it("check tokenOwner has all tokens", function() {
        var meta;
        var totalSupply;
        return MyAdvancedToken.deployed().then(function(instance) {
            meta = instance;
            return meta.totalSupply.call();
        }).then(function(_totalSupply) {
            totalSupply = _totalSupply.toNumber();
            return meta.getBalance(tokenOwnerAccount);
        }).then(function(bal) {
            return meta.totalSupply.call();
        }).then(function(_totalSupply) {
            assert.equal(_totalSupply.toNumber(), totalSupply, "all tokens should be ownwed by account[0] as contractor creator");
        });
    });

    it("test burn", function() {
        var burnVal = 2;
        var meta;
        return MyAdvancedToken.deployed().then(function(instance) {
            meta = instance;
            return meta.owner({from: tokenOwnerAccount});
        }).then(function() {
            return meta.totalSupply.call();
        }).then(function(totalSupply) {
            tokenTotalSupply = totalSupply.toNumber();
            return meta.burn(burnVal, {from: deployAccount});
        }).then(function() {
            return meta.totalSupply.call();
        }).then(function(_totalSupply) {
            assert.equal(_totalSupply.toNumber(), tokenTotalSupply - burnVal, "burned token should be reduced from totalSupply");
            tokenTotalSupply = _totalSupply.toNumber();
        })
    });
    
    it("test mint", function() {
        var mintVal = 2;
        var meta;
        return MyAdvancedToken.deployed().then(function(instance) {
            meta = instance;
            return meta.mintToken(tokenOwnerAccount, mintVal, {from: tokenOwnerAccount});
        }).then(function() {
            return meta.totalSupply.call();  
        }).then(function(_totalSupply) {
            assert.equal(_totalSupply.toNumber(), tokenTotalSupply + mintVal, "mint token should regain totalSupply");   
            tokenTotalSupply = _totalSupply.toNumber();
        });
    });

    // contract ---transfer--> auxiAccount
    it("test transfer", function() {
        var meta;
        var TRNSFER_AMOUNT = 1000;

        return MyAdvancedToken.deployed().then(function(instance) {

            meta = instance;
            return meta.getBalance.call(auxiAccount);

        }).then(function(bal) {

            tokenBalOfAux = bal.toNumber();
            return meta.getBalance.call(tokenOwnerAccount);

        }).then(function(bal) {

            tokenBalOfOwner = bal.toNumber();
            return meta.transfer(auxiAccount, TRNSFER_AMOUNT, {from: tokenOwnerAccount});

        }).then(function() {

            return meta.getBalance.call(auxiAccount);

        }).then(function(bal) {

            assert.equal(bal.toNumber(), tokenBalOfAux + TRNSFER_AMOUNT, "deployed account should accumulate balance");
            tokenBalOfAux = bal.toNumber();
            return meta.getBalance.call(tokenOwnerAccount);

        }).then(function(bal) {

            assert.equal(bal.toNumber(), tokenBalOfOwner - TRNSFER_AMOUNT, "owner account should decrease balance");
            tokenBalOfOwner = bal.toNumber();
        });
    });

    it("test setPrices", function() {
        var meta;
        return MyAdvancedToken.deployed().then(function(instance) {

            meta = instance;
            return meta.setPrices(SELLPRICE, BUYPRICE, {from: tokenOwnerAccount});

        }).then(function(resp) {

//            console.log("[set price response]:", resp);
            return meta.sellPrice.call();

        }).then(function(sellPrice) {

            assert.equal(sellPrice.toNumber(), SELLPRICE, "sellPrice should match setted value");
            return meta.buyPrice.call();

        }).then(function(_buyPrice) {

            assert.equal(_buyPrice.toNumber(), BUYPRICE, "buyPrice should match setted value");
        });
    })
    
    it("test sell", function() {
        var meta;
        var oldOwnerTokenBal;

        const SELL_AMOUNT = 10;

        return MyAdvancedToken.deployed().then(function(instance) {

            meta = instance;
            return meta.getBalance.call(MyAdvancedToken.address);

        }).then(function(tBal) {

            tokenBalOfContract = tBal.toNumber();
            // All tokens in tokenOwnerAccount initially, 
            // send 10 to contract
            return meta.sell(SELL_AMOUNT, {from: tokenOwnerAccount});

        }).then(function() {

            return web3.eth.getBalance(MyAdvancedToken.address);

        }).then(function(wBal) {

            assert.equal(wBal.toNumber(), contractWeiBal - SELL_AMOUNT * SELLPRICE, "After sell contract should reduce token-amount-WEI!");
            contractWeiBal = wBal.toNumber();
            return meta.getBalance.call(MyAdvancedToken.address);

        }).then(function(tBal) {

            assert.equal(tBal.toNumber(), tokenBalOfContract + SELL_AMOUNT, "After sell contract's token balance should be gained by sell amount");
            tokenBalOfContract = tBal.toNumber();
            return meta.getBalance.call(tokenOwnerAccount);

        }).then(function(tBal) {

            assert.equal(tBal.toNumber(), tokenBalOfOwner - SELL_AMOUNT, "After sell owner's token balance should be reduced by sell amount");
            tokenBalOfOwner = tBal.toNumber();
        });
    });

    it("test buy", function() {
        var meta;
        const BUY_WEI = 5e15;   // 5 tokens

        return MyAdvancedToken.deployed().then(function(instance) {

            meta = instance;
            return meta.buy({from: auxiAccount, value: BUY_WEI});

        }).then(function(resp) {

            return meta.getBalance.call(auxiAccount);

        }).then(function(bal) {

            assert.equal(bal.toNumber(), tokenBalOfAux + BUY_WEI/BUYPRICE, "After buy buyer's token balance should be added with right value");
            tokenBalOfAux = bal.toNumber();
            return meta.getBalance.call(MyAdvancedToken.address);

        }).then(function(bal) {

            assert.equal(bal.toNumber(), tokenBalOfContract - BUY_WEI/BUYPRICE, "After buy contract's balance should be reduced.");
            tokenBalOfContract = bal.toNumber();

        });
    });

    // Here VM Exception is right, cause account is freezed, then he can't buy anything
    it("test freeze", function() {
        var meta;

        return MyAdvancedToken.deployed().then(function(instance) {

            meta = instance;
            return meta.freezeAccount(auxiAccount, true, {from: tokenOwnerAccount});

        }).then(function() {

            return meta.buy({from: auxiAccount, value: 3});

        }).then(function() {
            console.log("after buy.");
        });
    });

    it("test unfreeze", function() {
        var meta;

        return MyAdvancedToken.deployed().then(function(instance) {

            meta = instance;
            meta.freezeAccount(auxiAccount, false, {from: tokenOwnerAccount});
        }).then(function() {
            meta.buy({from: auxiAccount, value: 3});
        });
    });

    it("test approve", function() {
        var meta;

        return MyAdvancedToken.deployed().then(function(instance) {

            meta = instance;
            // owner => auxi limit to 3
            return meta.approve(auxiAccount, 3, {from: tokenOwnerAccount});

        }).then(function() {

            return meta.transferFrom(tokenOwnerAccount, auxiAccount, 3, {from: auxiAccount});
        }).then(function() {

            tokenBalOfOwner -= 3;
            tokenBalOfAux += 3;

            return meta.approve(auxiAccount, APPROVE_LIMIT, {from: tokenOwnerAccount});
        }).then(function() {
            // To check if has error
        });
    });

    it("test burnFrom", function() {
        var meta;

        var burnFromValue = 10;

        return MyAdvancedToken.deployed().then(function(instance) {

            meta = instance;
            return meta.burnFrom(tokenOwnerAccount, burnFromValue, {from: auxiAccount});

        }).then(function() {

            return meta.getApprove.call(auxiAccount);

        }).then(function(val) {

            assert.equal(val.toNumber(), APPROVE_LIMIT-burnFromValue, "approve should be reduced after burnFrom");
            return meta.getBalance.call(tokenOwnerAccount);

        }).then(function(bal) {

            assert.equal(bal.toNumber(), tokenBalOfOwner-burnFromValue, "balance of 'from' account should be reduced after burnFrom");
            tokenBalOfOwner = bal.toNumber();
            return meta.totalSupply.call();

        }).then(function(_totalSupply) {

            assert.equal(_totalSupply.toNumber(), tokenTotalSupply-burnFromValue, "totalSupply should be reduced after burnFrom");

        });
    });

    it("test approveCall", function() {
        var meta;

        return MyAdvancedToken.deployed().then(function(instance) {

            meta = instance;
            console.log("1");
            // owner => auxi limit to 3
            return meta.approveAndCall(Crowdsale.address, 3, "ExtraData", {from: tokenOwnerAccount});

        }).then(function() {
            console.log("2");

        });
    });


            

    sleep(2000).then(() => {
        console.log("Sleep over!");
    });

});
