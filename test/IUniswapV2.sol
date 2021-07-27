pragma solidity >=0.5.0;

/* @gg:source
  name: Pair
  onInit:
    - UpdateField name
    - UpdateField symbol
    - UpdateField decimals
    - UpdateField totalSupply
    - UpdateField token0
    - UpdateField token1
*/
interface IUniswapV2Pair {
    event Approval(address indexed owner, address indexed spender, uint value);
    event Transfer(address indexed from, address indexed to, uint value);

    /* @gg:field */
    function name() external pure returns (string memory);
    
    /* @gg:field */
    function symbol() external pure returns (string memory);
    
    /* @gg:field */
    function decimals() external pure returns (uint8);
    
    /* @gg:field */
    function totalSupply() external view returns (uint);

    function balanceOf(address owner) external view returns (uint);
    function allowance(address owner, address spender) external view returns (uint);

    function approve(address spender, uint value) external returns (bool);
    function transfer(address to, uint value) external returns (bool);
    function transferFrom(address from, address to, uint value) external returns (bool);

    function DOMAIN_SEPARATOR() external view returns (bytes32);
    function PERMIT_TYPEHASH() external pure returns (bytes32);
    function nonces(address owner) external view returns (uint);

    function permit(address owner, address spender, uint value, uint deadline, uint8 v, bytes32 r, bytes32 s) external;

    /* @gg:handler 
        actions:
          - StoreEvent
          - UpdateField totalSupply
     */
    event Mint(address indexed sender, uint amount0, uint amount1);

    /* @gg:handler 
        actions:
          - StoreEvent
          - UpdateField totalSupply
     */
    event Burn(address indexed sender, uint amount0, uint amount1, address indexed to);

    /* @gg:handler 
        actions:
          - StoreEvent
          - UpdateField price0
          - UpdateField price1
     */
    event Swap(
        address indexed sender,
        uint amount0In,
        uint amount1In,
        uint amount0Out,
        uint amount1Out,
        address indexed to
    );
    event Sync(uint112 reserve0, uint112 reserve1);

    function MINIMUM_LIQUIDITY() external pure returns (uint);
    function factory() external view returns (address);

    /* @gg:field */
    function token0() external view returns (address);
    
    /* @gg:field */
    function token1() external view returns (address);
    function getReserves() external view returns (uint112 reserve0, uint112 reserve1, uint32 blockTimestampLast);

    /* @gg:field 
      name: price0 */
    function price0CumulativeLast() external view returns (uint);
    
    /* @gg:field 
      name: price1 */
    function price1CumulativeLast() external view returns (uint);

    function kLast() external view returns (uint);

    function mint(address to) external returns (uint liquidity);
    function burn(address to) external returns (uint amount0, uint amount1);
    function swap(uint amount0Out, uint amount1Out, address to, bytes calldata data) external;
    function skim(address to) external;
    function sync() external;

    function initialize(address, address) external;
}

/* @gg:source
  name: Factory
  instances:
    - address: '0x5C69bEe701ef814a2B6a3EDD4B1652CB9cc5aA6f'
      startBlock: 10000834
*/
interface IUniswapV2Factory {
    /* @gg:handler 
      actions:
        - NewEntity Pair from pair
    */
    event PairCreated(address indexed token0, address indexed token1, address pair, uint);

    function feeTo() external view returns (address);
    function feeToSetter() external view returns (address);

    function getPair(address tokenA, address tokenB) external view returns (address pair);
    function allPairs(uint) external view returns (address pair);
    function allPairsLength() external view returns (uint);

    function createPair(address tokenA, address tokenB) external returns (address pair);

    function setFeeTo(address) external;
    function setFeeToSetter(address) external;
}
