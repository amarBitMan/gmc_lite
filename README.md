# gmc_lite

An OTP application

## For Developers 

Set config value of data_root  as cwd 

### Build
    $ rebar3 compile

### Run
    $ rebar3 shell

### Release
        $ rebar3 release -n gmc_Lite   


## Installation
    1. clone repo
        gh repo clone amarBitMan/gmc_lite

    2. change directory to repo
        cd gmc_lite

    3. generate a docker build
        ./dockerz.sh build

    4. start container 
        ./dockerz.sh run

## Usage
You can use APIs mentioned on 
https://work.greyorange.com/confluence/display/BS/GMC-LITE
    
Make sure you place required files inside the folder `gmc_lite/data/`
