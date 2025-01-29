import { describe, expect, it } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const wallet1 = accounts.get("wallet_1")!;
const wallet2 = accounts.get("wallet_2")!;

describe("nft-marketplace contract", () => {
  it("successfully mints a new carbon credit", () => {
    const mintCall = simnet.callPublicFn(
      "nft-marketplace",
      "mint-credit",
      [Cl.uint(100), Cl.uint(20240129)],
      wallet1
    );
    expect(mintCall.result).toBeOk(Cl.uint(0));
  });

  it("allows transfer of carbon credits", () => {
    // First mint a credit
    const mintCall = simnet.callPublicFn(
      "nft-marketplace",
      "mint-credit",
      [Cl.uint(100), Cl.uint(20240129)],
      wallet1
    );
    
    // Then transfer it
    const transferCall = simnet.callPublicFn(
      "nft-marketplace",
      "transfer-credit",
      [Cl.uint(0), Cl.principal(wallet2)],
      wallet1
    );
    expect(transferCall.result).toBeOk(Cl.bool(true));
  });

  it("allows retirement of carbon credits", () => {
    // First mint a credit
    const mintCall = simnet.callPublicFn(
      "nft-marketplace",
      "mint-credit",
      [Cl.uint(100), Cl.uint(20240129)],
      wallet1
    );
    
    // Then retire it
    const retireCall = simnet.callPublicFn(
      "nft-marketplace",
      "retire-credit",
      [Cl.uint(0)],
      wallet1
    );
    expect(retireCall.result).toBeOk(Cl.bool(true));
  });
});