# haskalpaca

Work-In-Progress Haskell library for commission-free algorithmic trading with the Alpaca Markets API

## Installation
```
git clone https://github.com/RobbieBarnhoorn/haskalpaca.git
cd haskalpaca
stack build --haddock
```

## Working Integrations
- Authentication: Key-based API authentication
- Account: Get account information (cash, equity, long/short market value, etc.)
- Orders: Place, update, observe, and cancel orders
- Positions: Observe and liquidate open long/short positions

## Next
- Websocket streaming
- Market Data

## Example Library Usage
See [app/Main.hs](app/Main.hs)

Run the sample code with `stack run`
