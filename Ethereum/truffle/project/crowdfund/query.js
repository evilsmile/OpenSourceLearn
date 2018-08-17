var Crowdsale = artifacts.require("contracts/Crowsale.sol");

Crowdsale.deployed().then(function(instance) {
    var meta = instance;
    return meta.price.call()
    .then(function(val) {
        console.log("price: ", val);
    });
});
