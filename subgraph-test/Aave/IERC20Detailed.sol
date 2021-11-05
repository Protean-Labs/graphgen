// SPDX-License-Identifier: agpl-3.0
pragma solidity 0.6.12;

import {IERC20} from './IERC20.sol';

/* @gg:source 
  name: ERC20
*/
interface IERC20Detailed is IERC20 {
  /* @gg:field */
  function name() external view returns (string memory);

  /* @gg:field */
  function symbol() external view returns (string memory);

  /* @gg:field */
  function decimals() external view returns (uint8);

  /* @gg:field */
  function totalSupply() external view returns (uint256);
}
