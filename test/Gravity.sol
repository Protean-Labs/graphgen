pragma solidity ^0.4.0;

/** @gg
entity Gravatar {
  id: ID!
  owner: Bytes!
  displayName: String!
  imageUrl: String!
}

data_source Gravity is GravatarRegistry {
  addr = 0x2E645469f354BB4F5c8a05B3b30A929361cf77eC
  start_block = 1000923
}
*/
contract GravatarRegistry {
  /** @gg:handler
  new_entity Gravatar[event.params.id] {
    owner = event.params.owner
    displayName = event.params.displayName
    imageUrl = event.params.imageUrl
  }
  */
  event NewGravatar(uint id, address owner, string displayName, string imageUrl);

  /** @gg:handler
  update Gravatar[event.params.id] {
    owner = event.params.owner
    displayName = event.params.displayName 
    imageUrl = event.params.imageUrl
  }
  */
  event UpdatedGravatar(uint id, address owner, string displayName, string imageUrl);

  struct Gravatar {
    address owner;
    string displayName;
    string imageUrl;
  }

  Gravatar[] public gravatars;

  mapping (uint => address) public gravatarToOwner;
  mapping (address => uint) public ownerToGravatar;

  function createGravatar(string _displayName, string _imageUrl) public {
    require(ownerToGravatar[msg.sender] == 0);
    uint id = gravatars.push(Gravatar(msg.sender, _displayName, _imageUrl)) - 1;

    gravatarToOwner[id] = msg.sender;
    ownerToGravatar[msg.sender] = id;

    emit NewGravatar(id, msg.sender, _displayName, _imageUrl);
  }

  function getGravatar(address owner) public view returns (string, string) {
    uint id = ownerToGravatar[owner];
    return (gravatars[id].displayName, gravatars[id].imageUrl);
  }

  function updateGravatarName(string _displayName) public {
    require(ownerToGravatar[msg.sender] != 0);
    require(msg.sender == gravatars[ownerToGravatar[msg.sender]].owner);

    uint id = ownerToGravatar[msg.sender];

    gravatars[id].displayName = _displayName;
    emit UpdatedGravatar(id, msg.sender, _displayName, gravatars[id].imageUrl);
  }

  function updateGravatarImage(string _imageUrl) public {
    require(ownerToGravatar[msg.sender] != 0);
    require(msg.sender == gravatars[ownerToGravatar[msg.sender]].owner);

    uint id = ownerToGravatar[msg.sender];

    gravatars[id].imageUrl =  _imageUrl;
    emit UpdatedGravatar(id, msg.sender, gravatars[id].displayName, _imageUrl);
  }

  // the gravatar at position 0 of gravatars[]
  // is fake
  // it's a mythical gravatar
  // that doesn't really exist
  // dani will invoke this function once when this contract is deployed
  // but then no more
  function setMythicalGravatar() public {
    require(msg.sender == 0x8d3e809Fbd258083a5Ba004a527159Da535c8abA);
    gravatars.push(Gravatar(0x0, " ", " "));
  }
}
