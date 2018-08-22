var Crowdsale = artifacts.require('./Crowdsale.sol')
var MyAdvancedToken = artifacts.require('./MyAdvancedToken.sol')

module.exports = function (deployer, network, accounts) {

    var tokenOwnerAddr = accounts[0];
    const CROWDSALE_INIT_TOKENS = 400000;

    return deployer.then(() => {

        const initialSupply = 100000;
        const tokenName = "MyAdCoin";
        const tokenSymbol = "MACr";

        return deployer.deploy(MyAdvancedToken, initialSupply, tokenName, tokenSymbol, {from: tokenOwnerAddr});
    }).then(() => {

        const ifSuccessfulSendTo = web3.eth.accounts[5];
        const fundingGoalInEthers = 10;
        // 30 MIN
        var durationInSeconds = 30 * 60;
        //var durationInSeconds = 3 * 60;

        if (network == "test") {
            durationInSeconds = 1;
        }

        const etherCostOfEachToken = 1e15 ;
        const addressOfTokenUsedAsReward = MyAdvancedToken.address;

        // Deploy and charge.
        return deployer.deploy(Crowdsale, ifSuccessfulSendTo, fundingGoalInEthers, durationInSeconds, etherCostOfEachToken, addressOfTokenUsedAsReward, 
             {from:accounts[0], value:1e17});

    }).then(() => {
        return Crowdsale.deployed().then( instance => {
            console.log("Transfer initial tokens to contract address!");
            let tokenIns;

            return MyAdvancedToken.deployed().then( _tokenIns => {
                tokenIns = _tokenIns;
                return tokenIns.approve(Crowdsale.address, CROWDSALE_INIT_TOKENS);
            }).then(function() {
                return tokenIns.transfer(Crowdsale.address, CROWDSALE_INIT_TOKENS);
            }).then(function() {
                return tokenIns.balanceOf.call(Crowdsale.address);   
            }).then(function(bal) {
                if (!bal) {
                    console.log("ERROR transfer tokens to crowdsale address.");
                } else {
                    console.log("Crowdsale address get ", bal.valueOf(), " tokens.");
                }
            });
        }).catch(err => {
            console.log(err);
        });
    });
}
