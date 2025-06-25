# 🏆 Skilltag - Verified Skill Badges NFT Contract

A Clarity smart contract for creating and managing verified skill badges as NFTs on the Stacks blockchain. Perfect for educational institutions, course providers, and organizations looking to issue verifiable digital credentials.

## 🌟 Features

- 🎓 **Mint Skill Badges**: Create NFTs representing verified skills and certifications
- 👥 **Authorized Issuers**: Control who can mint skill badges through authorization system
- ⏰ **Expiring Credentials**: Set validity periods for skill badges with automatic expiry
- 🏪 **Marketplace**: Built-in marketplace for trading skill badges
- 📊 **Skill Tracking**: Track user skills and proficiency levels
- 🔍 **Verification**: Validate skill badges with verification hashes
- 🔥 **Auto-Burn**: Automatically remove expired credentials

## 🚀 Getting Started

### Prerequisites
- Clarinet installed
- Stacks wallet for testing

### Installation

```bash
clarinet new skilltag-project
cd skilltag-project
```

Replace the contents of `contracts/Skilltag.clar` with the provided contract code.

## 📖 Usage

### Authorize Skill Issuers
```clarity
(contract-call? .Skilltag authorize-issuer 'SP1234...)
```

### Mint a Skill Badge
```clarity
(contract-call? .Skilltag mint-skilltag 
  'SP1234...  ;; recipient
  "JavaScript"  ;; skill name
  "Advanced"  ;; skill level (Beginner/Intermediate/Advanced/Expert)
  "Codecademy"  ;; course provider
  "abc123..."  ;; verification hash
  u52560)  ;; validity in blocks (~1 year)
```

### List Badge for Sale
```clarity
(contract-call? .Skilltag list-in-ustx u1 u1000000 u1000)
```

### Buy a Skill Badge
```clarity
(contract-call? .Skilltag buy-in-ustx u1 'SP1234...)
```

### Check User Skills
```clarity
(contract-call? .Skilltag get-user-skills 'SP1234...)
```

### Validate Badge
```clarity
(contract-call? .Skilltag validate-skilltag u1)
```

## 🎯 Skill Levels

- **Beginner**: Entry-level proficiency
- **Intermediate**: Moderate proficiency  
- **Advanced**: High proficiency
- **Expert**: Master-level proficiency

## 🔧 Contract Functions

### Public Functions
- `authorize-issuer` - Authorize entities to mint skill badges
- `mint-skilltag` - Create new skill badge NFT
- `transfer` - Transfer badge ownership
- `list-in-ustx` - List badge on marketplace
- `buy-in-ustx` - Purchase listed badge
- `burn-expired-skilltag` - Remove expired badges

### Read-Only Functions
- `get-skilltag-data` - Get badge metadata
- `get-user-skills` - Get user's skill list
- `validate-skilltag` - Check if badge is still valid
- `get-skills-by-level` - Filter skills by proficiency level

## 💡 Use Cases

- 🎓 **Educational Institutions**: Issue verified diplomas and certificates
- 💼 **Corporate Training**: Track employee skill development
- 🏅 **Professional Certifications**: Create industry-recognized credentials
- 🎮 **Skill-based Gaming**: Reward players with verified achievements
- 📚 **Online Courses**: Provide verifiable completion certificates

## 🛡️ Security Features

- Owner-only administrative functions
- Authorized issuer system
- Expiry-based credential management
- Verification hash for authenticity
- Commission-based marketplace with configurable rates

## 📝 Testing

```bash
clarinet test
```

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

