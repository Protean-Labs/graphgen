interface ICuration {
  // Events

  event Signalled(
    address indexed curator,
    bytes32 indexed subgraphDeploymentID,
    uint256 tokens,
    uint256 signal,
    uint256 curationTax
  );

  event Burned(
    address indexed curator,
    bytes32 indexed subgraphDeploymentID,
    uint256 tokens,
    uint256 signal
  );

  event Collected(bytes32 indexed subgraphDeploymentID, uint256 tokens);

  // -- Configuration --

  function setDefaultReserveRatio(uint32 _defaultReserveRatio) external;

  function setMinimumCurationDeposit(uint256 _minimumCurationDeposit) external;

  function setCurationTaxPercentage(uint32 _percentage) external;

  // -- Curation --

  function mint(
      bytes32 _subgraphDeploymentID,
      uint256 _tokensIn,
      uint256 _signalOutMin
  ) external returns (uint256, uint256);

  function burn(
      bytes32 _subgraphDeploymentID,
      uint256 _signalIn,
      uint256 _tokensOutMin
  ) external returns (uint256);

  function collect(bytes32 _subgraphDeploymentID, uint256 _tokens) external;

  // -- Getters --

  function isCurated(bytes32 _subgraphDeploymentID) external view returns (bool);

  function getCuratorSignal(address _curator, bytes32 _subgraphDeploymentID)
      external
      view
      returns (uint256);

  function getCurationPoolSignal(bytes32 _subgraphDeploymentID) external view returns (uint256);

  function getCurationPoolTokens(bytes32 _subgraphDeploymentID) external view returns (uint256);

  function tokensToSignal(bytes32 _subgraphDeploymentID, uint256 _tokensIn)
      external
      view
      returns (uint256, uint256);

  function signalToTokens(bytes32 _subgraphDeploymentID, uint256 _signalIn)
      external
      view
      returns (uint256);

  function curationTaxPercentage() external view returns (uint32);
}