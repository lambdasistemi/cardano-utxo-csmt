---
marp: true
theme: default
class: invert
paginate: true
---

# Poor Man's Light Clients for Cardano

### Trust-minimized blockchain access via Merkle proofs

---

## The Challenge: Full Nodes Are Heavy

**Today's Reality**

- Running a Cardano full node requires **~300GB** of storage
- High memory requirements
- Constant network availability and bandwidth

**Tomorrow's Reality**

- Storage requirements will only grow
- Hardware requirements will rise with new features
- Amaru will help a bit, but not enough

**This kills participation**

---

## Why Does This Matter?

**Decentralization depends on accessibility**

- Mobile wallets can't run full nodes
- Browsers can't run full nodes
- IoT devices can't run full nodes
- Developing countries face infrastructure barriers

**Result:** Most users depend on centralized services for blockchain data and transaction building.

---

## The Trust Problem

**Application servers provide blockchain data and unsigned transactions for users to interact with the network.**

- Without a full node, users must trust the server
- Trusting a centralized server undermines decentralization
- Even honest servers bring risks: hackers can compromise them
- Users have no way to verify data integrity

---

**Current Light Client Options**

| Approach | Trust Required |
|----------|---------------|
| Centralized API | Trust the provider completely |
| Full node | Trust no one (but heavy) |

**We need a middle ground**

---

## We Can Prove an Untrusted Server Is Honest

**Cryptographic Proofs Remove Trust Assumptions from servers of web3 applications.**

If untrusted servers provide proofs that clients can verify independently, we remove the need to trust them:
- **Good for the server**: they're protected even if hacked or compromised
- **Good for the client**: they trust math, not promises
- **Good for trusted parties**: they build reputation without involvement in specific applications

---

## The Solution: Merkle Trees

**A Fingerprint for the Entire UTxO Set**

```
        Root Hash (32 bytes)
       /                    \
    Hash                    Hash
   /    \                  /    \
 UTxO  UTxO              UTxO  UTxO
```

- A single root hash represents **millions of UTxOs**
- Anyone can verify that a UTxO belongs to the set
- Changing any UTxO changes the root

---

## The Simplest Possible Infrastructure

**Publishing a Merkle root is trivial**

A JSON file with UTxO set root hashes covering the rollback window, updated every block (~20 seconds):

```json
[
  { "slot": 123456789, "root": "a1b2c3..." },
  { "slot": 123456769, "root": "d4e5f6..." },
  { "slot": 123456749, "root": "g7h8i9..." },
  ...
]
```

**Can be hosted anywhere:**
- GitHub release asset
- Google Drive
- S3 bucket
- Even a tweet/X post

---

## Poor Man's Multisignature

**Multiple independent publishers = distributed trust**

<div style="margin: 1em auto; text-align: center;">
<!-- publishers --><img src="https://mermaid.ink/svg/JSV7aW5pdDogeyJ0aGVtZSI6ICJkYXJrIiwgInRoZW1lVmFyaWFibGVzIjogeyJlZGdlTGFiZWxCYWNrZ3JvdW5kIjogInRyYW5zcGFyZW50In19fSUlCmZsb3djaGFydCBURAogICAgc3ViZ3JhcGggVHJ1c3RlZCBSb290IFB1Ymxpc2hlcnMKICAgICAgICBDRltDYXJkYW5vIEZvdW5kYXRpb25dCiAgICAgICAgSU9HW0lPR10KICAgICAgICBFbXVyZ29bRW11cmdvXQogICAgICAgIE1pdGhyaWxbTWl0aHJpbF0KICAgIGVuZAogICAgQ0YgLS0-IFIxW3Jvb3Qgb2YgY2hhaW4tcG9pbnQgJ2FiNDUzYi4uLiddCiAgICBJT0cgLS0-IFIyW3Jvb3Qgb2YgY2hhaW4tcG9pbnQgJ2FiNDUzYi4uLiddCiAgICBFbXVyZ28gLS0-IFIzW3Jvb3Qgb2YgY2hhaW4tcG9pbnQgJ2FiNDUzYi4uLiddCiAgICBNaXRocmlsIC0tPiBSNFtyb290IG9mIGNoYWluLXBvaW50ICdhYjQ1M2IuLi4nXQogICAgUjEgJiBSMiAmIFIzICYgUjQgLS0-IFZ7QWxsIG1hdGNoP30KICAgIFYgLS0-fFllc3wgVFtDbGllbnQgYWNjZXB0cyB2YWxpZCBwcm9vZnMgZm9yIHRoZSByb290ICdhYjQ1M2IuLi4nXQo=" style="max-height: 350px;">
</div>

**Light clients sample 2-3 sources:** match → verified | differ → investigate

---

## Who Should Publish?

**Institutions (low effort, high impact)**

| Publisher | Why |
|-----------|-----|
| Cardano Foundation | Ecosystem steward |
| IOG | Core developers |
| Emurgo | Enterprise adoption |
| Mithril team | Already running infra |

**Anyone else**

- Stake pool operators
- Exchanges
- dApp developers
- Community members
- Any company using Cardano for business operations

**More publishers = stronger decentralization**

---

## The Architecture

<div style="margin: 1em auto; text-align: center;">
<!-- architecture --><img src="https://mermaid.ink/svg/JSV7aW5pdDogeyJ0aGVtZSI6ICJkYXJrIiwgInRoZW1lVmFyaWFibGVzIjogeyJlZGdlTGFiZWxCYWNrZ3JvdW5kIjogInRyYW5zcGFyZW50In19fSUlCmZsb3djaGFydCBMUgogICAgc3ViZ3JhcGggQ2xpZW50cyBbTGlnaHQgQ2xpZW50c10KICAgICAgICBNb2JpbGVbTW9iaWxlXQogICAgICAgIEJyb3dzZXJbQnJvd3Nlcl0KICAgICAgICBJb1RbSW9UXQogICAgZW5kCiAgICBzdWJncmFwaCBXZWIyIFtXZWIyIFBsYXRmb3Jtc10KICAgICAgICBHaXRIdWJbR2l0SHViXQogICAgICAgIFhbVHdpdHRlci9YXQogICAgICAgIENETltDRE5dCiAgICBlbmQKICAgIHN1YmdyYXBoIFB1Ymxpc2hlcnMgW01lcmtsZSBSb290IFB1Ymxpc2hlcnNdCiAgICAgICAgTWl0aHJpbFtNaXRocmlsXQogICAgICAgIFNQT3NbU1BPc10KICAgICAgICBDRltGb3VuZGF0aW9uXQogICAgICAgIElPR1tJT0ddCiAgICBlbmQKICAgIHN1YmdyYXBoIFNlcnZlcnMgW0FwcGxpY2F0aW9uIFNlcnZlcnNdCiAgICAgICAgTVBGU1tNUEZTXQogICAgICAgIEJsb2NrZnJvc3RbQmxvY2tmcm9zdF0KICAgICAgICBLb2lvc1tLb2lvc10KICAgIGVuZAogICAgUHVibGlzaGVycyAtLT58cHVibGlzaCByb290c3wgV2ViMgogICAgQ2xpZW50cyAtLT58MS4gRmV0Y2ggcm9vdHN8IFdlYjIKICAgIENsaWVudHMgLS0-fDIuIFJlcXVlc3QgZGF0YSArIHByb29mc3wgU2VydmVycwogICAgU2VydmVycyAtLT58Y2FyZGFuby11dHhvLWNzbXR8IEJDWyhDYXJkYW5vIEJsb2NrY2hhaW4pXQogICAgUHVibGlzaGVycyAtLT58Y2FyZGFuby11dHhvLWNzbXR8IEJDCg==" style="max-height: 380px;">
</div>

---

## Benefits Summary

| Stakeholder | Benefit |
|-------------|---------|
| **End Users** | Fast, verifiable access to dApps |
| **Developers** | Simple API, no node required, just proof verification libraries |
| **Ecosystem** | More decentralized infrastructure |
| **Publishers** | Low effort, high impact reputation boost |

---

## Cost to Participate

| Role | Infrastructure | Interface |
|------|----------------|-----------|
| **Root Publisher** | Node + cardano-utxo-csmt | Static file hosting (GitHub, S3, etc.) |
| **Application Server** | Node + cardano-utxo-csmt | Patch API to serve proofs with data |
| **Light Client** | None | Proof verification library (browser/mobile) |

**Publishing roots costs almost nothing**

---

## Current Status

**Working Today**

- ✅ Merkle tree construction from UTxO set
- ✅ Proof generation and verification
- ✅ HTTP API for light clients
- ✅ Fast bootstrap via genesis + chain sync
- ✅ Real-time chain sync

**Next Steps**

- Integration with wallet SDKs
- Public testnet deployment
- Performance optimization

---

## The Vision

**A Cardano where:**

- Application servers don't bear the burden of trust
- Light clients choose their trusted publishers from multiple independent institutions

---

## Questions?

**Resources**

- GitHub: `lambdasistemi/cardano-utxo-csmt`
- Documentation: `/docs`
