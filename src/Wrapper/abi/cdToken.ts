export default [
  "function pair() view returns (address)",
  "function balanceOf(address) view returns (uint256)",
  "function dueOf(uint256) view returns ((uint112,uint112,uint32))",
  "function tokenOfOwnerByIndex(address,uint256) view returns (uint256)",
  "function setApprovalForAll(address, bool)",
  "function isApprovedForAll(address, address) view returns (bool)",
  "event Transfer(address indexed,address indexed,uint256 indexed)",
];
