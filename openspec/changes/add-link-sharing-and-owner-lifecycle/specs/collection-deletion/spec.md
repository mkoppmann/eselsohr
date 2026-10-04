## Purpose

Defines owner-only deletion of a whole collection through the web interface, what deletion stops, and what it does not erase.

## ADDED Requirements

### Requirement: Owner-only collection deletion
Deleting a whole collection SHALL require the collection's root bearer and an explicit POST request through the web interface.
This authority MUST NOT be delegable: no child link, even with every permission, can delete the collection.

#### Scenario: Owner deletes the collection
- **WHEN** the owner submits the deletion form with the root bearer
- **THEN** the collection is deleted and the response is a receipt that does not depend on the now-invalid owner URL

#### Scenario: Delegated holder with every permission
- **WHEN** a holder of a child link with all eight permissions requests deletion
- **THEN** the request is denied and the collection is unchanged

### Requirement: Deleted collections stop authorizing requests
Once deletion commits, every bearer for the collection, including the owner's, MUST be denied.

#### Scenario: Request after deletion
- **WHEN** any link of a deleted collection is presented after deletion commits
- **THEN** the request is denied

### Requirement: Live databases are not unlinked
Deletion MUST NOT unlink a collection's database file while other requests may still hold it.

#### Scenario: Concurrent request during deletion
- **WHEN** another request holds the collection's database while the owner deletes the collection
- **THEN** that request's database file is not removed from under it

### Requirement: Deletion does not erase backups
Collection deletion SHALL NOT be described as erasing backups or copies.
File removal and backup retention MUST be explained separately from logical deletion, and restoring an old backup can restore the collection and its links.

#### Scenario: Deletion receipt
- **WHEN** the owner deletes a collection
- **THEN** the receipt and operator guidance make no claim that backups or copies were erased
