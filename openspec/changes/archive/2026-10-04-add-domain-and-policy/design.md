# Design

## Context

### Legacy authorization evidence

[Capability.hs](../../../../src/Lib/Domain/Capability.hs) defines `OverviewPerms`, `ArticlesPerms`, and `ArticlePerms` as records of `Maybe` singleton values that derive `Ord`.
`createSharedOverviewRef`, `createSharedArticlesRef`, and the article-to-article case of `createSharedArticleRef` use `ogPerms >= newPerms`, which compares records lexicographically, not by permission inclusion.
For example, an article-list parent with Read and Share but no Add compares greater than a child without Read but with Add: the first field decides the comparison.
Giving up an earlier permission can therefore mask acquiring a later one, a concrete defect in those attenuation helpers.
The list-to-article case instead checks individual Boolean pairs and does implement pointwise presence inclusion.

[Authorization.hs](../../../../src/Lib/Domain/Authorization.hs) hides the constructors of permission witnesses such as `ChangeTitlePerm`.
Some contain an article ID that downstream functions extract rather than accepting a second ID, a useful compile-time connection between an operation and its target.
The local alias `Authorized a` is only `Either AppErrorType a`, not a generic sealed authorized-action representation.
The witnesses do not prove possession of an authentic bearer token: their policy inputs (`Capability`, `ObjectReference`, permission records) have public constructors.
They bind no collection, expiration, ancestry, current database state, or transaction.

[ArticleList.hs](../../../../src/Lib/Domain/ArticleList.hs) illustrates both sides: `changeArticleTitle` consumes the witness's ID, while `addArticle` ignores its permission value.
`fromMap` and `toMap` expose the underlying representation, and the [repository interface](../../../../src/Lib/Domain/Repo/ArticleList.hs) accepts a separate collection ID and arbitrary article-list transformations.
These APIs do not make bypassing policy impossible throughout the application.

The existing `canChangeArticleTitle` tests the write permission alone, and [EditArticle.query](../../../../src/Lib/Ui/Web/Page/EditArticle.hs) checks that write permission and then loads the article title without a separate Read check.
Eselsohr 2's Read requirement for renaming and read-state changes is a redesign requirement, not a guarantee of the legacy policy.

### Legacy domain values

[Article](../../../../src/Lib/Domain/Article.hs) already has a small `Unread | Read` type and pure title and state transformations worth keeping.
[NonEmptyText](../../../../src/Lib/Domain/NonEmptyText.hs) hides its constructor and rejects empty text, though not whitespace-only text, and its error argument couples validation to the general application error.
[Id](../../../../src/Lib/Domain/Id.hs) uses a phantom parameter to distinguish ID kinds in ordinary signatures; the exported constructor and phantom role are not an unforgeability boundary.
`Eq Article` and `Eq Capability` compare IDs only, which can hide field changes in tests that intend structural equality.

### Legacy tests

The [authorization tests](../../../../test/Test/Domain/Authorization.hs) contain 815 lines of mostly example combinations.
Their behavioral questions about missing rights, exact targets, Read plus edit permissions, owner-only deletion, and delegation bounds are preserved, but not the faulty attenuation comparison as expected behavior.
Some legacy property generators use real random IO rather than explicit shrinkable inputs; see the [token properties](../../../../test/Test/Ui/Web/Dto/Accesstoken.hs).

## Goals / Non-Goals

**Goals:**

- Values and policy whose laws can be checked exhaustively or with bounded generators.
- A clear line between valid policy data and authentic capability possession.

**Non-Goals:**

- Any claim that pure checks establish remote authority; possession, decision, state transition, and persistence remain distinct.

## Decisions

### Validated values

The identifier, title, and URL types are abstract with total constructors returning `Either`, explicit text renderers, and small input errors.

```haskell
parseCollectionId :: Text -> Either IdError CollectionId
mkTitle :: Text -> Either TitleError Title
parseWebUrl :: Text -> Either WebUrlError WebUrl
```

`network-uri` validates URI syntax, and `idna2008` validates domain labels with strict IDNA2008 checks and case and NFC mappings.
Random identifier generation belongs to trusted creation and issuance code, not to these values.

### Article transitions

`Article` is a public record of five fields; transitions are total functions over validated values.

```haskell
newArticle :: ArticleId -> Title -> WebUrl -> UTCTime -> Article
rename :: Title -> Article -> Article
setReadState :: ArticleState -> Article -> Article
```

Persistence ignorance is an agreed constraint: the protected Store owns concurrency checks, and the persistence adapter owns storage representations and their bounds.
Pure transformations take no authorization argument.

### One permission set

Capabilities have one permission set, including permission to delegate, rather than separate sets for use and granting.
Indirect authority is compatible with object capabilities: a holder who can mint a Read capability can obtain Read authority, even if an API calls this only management.
Separate use and grant sets would make that distinction easy to misdescribe and would add policy states without confining the holder.
`Ord Permission` serves set storage only; scopes have no `Ord` instance.

```haskell
data Permission
  = ReadArticles | AddArticles | ChangeTitles | ChangeReadState
  | DeleteArticles | Delegate | InspectLinks | RevokeLinks
  deriving stock (Eq, Ord, Enum, Bounded, Show)
type Permissions = Set Permission

data Scope
  = WholeCollection CollectionId
  | OneArticle CollectionId ArticleId
  deriving stock (Eq, Show)

permissionsSubsetOf :: Permissions -> Permissions -> Bool
scopeSubsetOf :: Scope -> Scope -> Bool
```

### Bounded delegation

Delegation is recursive and limited to four edges from the root at depth zero.
The root has all eight permissions and no expiration, and every child has a finite deadline; equality with the deadline is already expired.
The seven-day default for everyday links belongs to later issuance work, not the validator.

```haskell
data GrantSnapshot = GrantSnapshot
  { grantId :: CapabilityId
  , grantParentId :: Maybe CapabilityId
  , grantScope :: Scope
  , grantPermissions :: Permissions
  , grantExpiresAt :: Maybe UTCTime
  , grantDepth :: Int
  }
  deriving stock (Eq, Show)

maxDelegationDepth :: Int
maxDelegationDepth = 4

attenuates :: UTCTime -> GrantSnapshot -> GrantSnapshot
           -> Either AttenuationError ()

validateChain :: UTCTime -> CollectionId -> CapabilityId -> [GrantSnapshot]
              -> Either ChainError ValidChain
chainGrants :: ValidChain -> NonEmpty GrantSnapshot
chainCheckedAt :: ValidChain -> UTCTime

validateGrantStructure :: CollectionId -> CapabilityId -> [GrantSnapshot]
                       -> Either ChainError (NonEmpty GrantSnapshot)
```

`GrantSnapshot` has a public constructor and contains no bearer secret or separate owner flag; collection identity comes from its scope.
There is no reparenting operation, and the store must keep persisted parent links immutable.
`ValidChain` has a private constructor, read-only accessors, structural `Eq`, and `Show`, with no record-update fields, `Read`, `Generic`, or unchecked reconstruction API.
The size check bounds the list traversal, not the evaluation of arbitrary undefined Haskell values.

### Operation policy

```haskell
decide :: UTCTime -> ValidChain -> Operation -> Either PolicyError ()
```

Operation values name the requested action and target, without carrying titles, URLs, read-state values, or revisions; revision metadata belongs to the Store.
Pure decisions do not perform protected effects or construct the Store's sealed requests.

| Operation | Required policy |
| --- | --- |
| ReadArticlesIn scope | ReadArticles and containment of the requested scope in the acting scope. |
| AddArticle collection | AddArticles on WholeCollection for that collection. |
| RenameArticle collection article | ReadArticles and ChangeTitles on that article. |
| ChangeArticleReadState collection article | ReadArticles and ChangeReadState on that article. |
| DeleteArticle collection article | DeleteArticles on that article; ReadArticles is not required. |
| IssueChild child | Delegate and a valid complete chain with the proposed child prepended. |
| InspectLink target path | InspectLinks and a proper descendant relationship; the root owner may also inspect its own root metadata. |
| RevokeLink target path | RevokeLinks and a proper descendant relationship; the root cannot be revoked through this operation. |
| DeleteCollection collection | A root-only acting chain for that collection. |

### Named operations as typed functions

Organize the application around named operations: the meaningful things a holder can do, such as adding an article, changing its title or read state, deleting it, delegating, and revoking a link.
Create, update, and delete on stored records are not the organizing unit.
Validated values and meaningful states belong in types, decisions in pure functions, and each operation in a typed function whose effects name only the authority it uses.
Derive mechanical representations, such as row DTOs, typed routes and links, and decoding of raw form input, but not domain behavior, and never bypass validated constructors.
Introduce a shared abstraction only after repeated operations show that it removes real duplication.

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Application model | CRUD repositories; a declarative resource/action framework such as Ash; named operations as typed functions | Prefer named operations with plain effect constraints. Declarative frameworks derive interfaces from operation descriptions, but add machinery each reader must learn; derive only mechanical representations. |

Frameworks such as Elixir's Ash declare such operations and derive interfaces from those declarations; Eselsohr expresses them as ordinary Haskell types and functions instead of a resource/action language.

### Tests

- Article tests check construction, all five equality fields, field preservation, no-ops, idempotence, and restoration after toggling twice.
- Permission tests exhaust all 65,536 pairs of the 256 subsets and all 65,536 nested subset chains.
- Scope tests cover inclusion, structural equality, and order laws over two collections and three article IDs reused across them.
- Attenuation tests cover root and child fields, ordered failures, deadline and depth boundaries, and generated valid or malformed parent-child pairs.
- Ancestry tests cover complete paths in generated trees, corrupted or missing ancestors, collection binding, deadline and depth boundaries, and bounded traversal of infinite lists.
- Operation tests cover the table over all 256 permission sets and monotonicity over all 6,561 subset pairs, each across whole-collection and two article scopes, plus generated trees, ancestors allowed whatever their descendants are allowed, expired targets, actor expiration rechecks, conflicting fields for shared ancestors, and issuance failures.
- External consumer compilation checks confirm private construction of identifiers, titles, URLs, and `ValidChain`, with a compiling positive control.

Pure tests exercise policy; source review checks exports, payload types, and roles.
Neither replaces HTTP and database tests.

## Risks / Trade-offs

- [Grant records are publicly constructible] → Only chain validation inside a protected operation's transaction turns stored grants into authority; pure success establishes policy validity only.
- [A retained `ValidChain` misses later expiration and revocation] → `decide` revalidates at its decision time, and protected operations reload grants in every transaction.
- [Exhaustive tests cover finite vocabularies only] → Scoped and ancestry properties use bounded generators; they provide evidence, not proof.
