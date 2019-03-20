# Switchboard

Control your computer configuration, VPNs, virtual machines via
switches.

[![asciicast](https://asciinema.org/a/2nBBQSXcgaoXQOjakUmeqNCrg.png)](https://asciinema.org/a/2nBBQSXcgaoXQOjakUmeqNCrg)

## Installation

1. [Install Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/),
   make sure that the path is set up on your machine as described in the Stack install docs.
2. Clone this repository: `git clone https://github.com/mpetruska/switchboard.git`
3. Navigate to the repository base directory: `cd switchboard`
4. On NixOS: `stack install` vs non-NixOS: `stack --no-nix install`
5. The `switchboard` executable should be installed and available in your path.

## Updating

1. Navigate to the repository base directory: `cd switchboard`
2. `git pull`
3. `stack install` as described above.

## Usage

- `switchboard`: reads switches configuraiton from the `switchboard.yaml` file found
                 in the current directory
- `switchboard -f someother.yaml`: reads configuration from `someother.yaml`

## Keybindings

|                     |                              |
|---------------------|------------------------------|
| up/down             | select previous/next switch  |
| left, right, space  | flip selected switch         |
| l                   | show switch command log      |
| q                   | quit                         |

## Configuration

Switches can be configured in yaml. Example:

    switches:
    
      - title:      VPN1
        initialize: "false"
        on:         "~/project1/vpn connect"
        off:        "~/project1/vpn disconnect""
    
      - title:      VPN2
        initialize: "ping -c3 jira.p2.local"
        on:         "~/p2/barracuda_connect.sh"
        off:        "~/p2/barracuda_disconnect.sh"
    
      - title:      Vagrant project1
        initialize: "false"
        on:         "pushd ~/project1/GitHub/work; vagrant up; popd"
        off:        "pushd ~/project1/GitHub/work; vagrant halt; popd"

