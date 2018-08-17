var Crowdsale = artifacts.require('./Crowdsale.sol')
var MyAdvancedToken = artifacts.require('./MyAdvancedToken.sol')

module.exports = function (deployer, network, accounts) {
    return deployer.then(() => {

        const initialSupply = 100000;
        const tokenName = "MyAdCoin";
        const tokenSymbol = "MACr";

        return deployer.deploy(MyAdvancedToken, initialSupply, tokenName, tokenSymbol, {from: accounts[0]});
    }).then(() => {

        const ifSuccessfulSendTo = web3.eth.accounts[5];
        const fundingGoalInEthers = 1;
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
             {from:accounts[0], value:1e18});

    }).then(() => {
        return Crowdsale.deployed().then( instance => {
            //console.log(instance);
            console.log("Deployed success!");
        }).catch(err => {
            console.log(err);
        });
    });
}
