package scalan.effects

import java.io.File

import scalan._
import scalan.compilation.{Compiler, GraphVizConfig}
import scalan.compilation.lms._
import scalan.compilation.lms.cxx.LmsCompilerCxx
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.compilation.lms.uni.LmsCompilerUni
import scalan.examples._
import scalan.it.BaseItTests
import scalan.monads.{MonadsDsl, MonadsDslExp}
import scalan.primitives.EffectfulCompiler

trait EffectsProg extends ScalanCommunityDsl with InteractionsDsl with InteractExample

class EffectsItTests extends BaseItTests[EffectsProg](new ScalanCommunityDslSeq with EffectsProg with InteractionsDslSeq) {

  class EffectsExp extends ScalanCommunityDslExp with EffectsProg with InteractionsDslExp with JNIExtractorOpsExp

  val progInteractScala = new CommunityLmsCompilerScala(new EffectsExp) with CoreBridge with EffectfulCompiler[EffectsExp]

  // TODO code generation for LmsCompilerUni fails with "Type Unit cannot be remapped"
  // val progInteractUni = new LmsCompilerUni(new EffectsExp) with CoreBridge with EffectfulCompiler[EffectsExp]

  val defaultCompilers = compilers(progInteractScala/*, progInteractUni*/)

  test("runInteract") {
    getStagedOutput(_.runAppW)(10)
  }

  test("runInteract2") {
    getStagedOutput(_.runApp2W)(10)
  }
}

trait State0Prog extends ScalanCommunityDsl with StateExamples with MonadsDsl

class StateItTests extends BaseItTests[State0Prog](???) {
  
  class ScalanState0Exp extends ScalanCommunityDslExp with State0Prog with MonadsDslExp with JNIExtractorOpsExp {
    val State = new State0Manager[Int]
  }

  val progState0Scala = new CommunityLmsCompilerScala(new ScalanState0Exp) with CoreBridge with EffectfulCompiler[ScalanState0Exp]

  val progState0Uni = new LmsCompilerUni(new ScalanState0Exp) with CoreBridge with EffectfulCompiler[ScalanState0Exp]

  val defaultCompilers = compilers(progState0Scala, progState0Uni)

  val in = Array(10.0, 20.0, 30.0)

  test("zipArrayWithIndex")  {
    val Seq(Seq(res, resU)) = getStagedOutput(_.zipArrayWithIndexW)(in)

    assert(res.sameElements(resU))
  }

  test("zipCollectionWithIndex")  {
    val Seq(Seq(res, resU)) = getStagedOutput(_.zipCollectionWithIndexW)(in)

    assert(res.sameElements(resU))
  }

  test("zipCollectionWithIndex2")  {
    //pending
    val Seq(Seq(res, resU)) = getStagedOutput(_.zipCollectionWithIndexW2)(in)

    assert(res.sameElements(resU))
  }

  test("zipCollectionWithIndex3")  {
    //pending
    val Seq(Seq(res, resU)) = getStagedOutput(_.zipCollectionWithIndexW3)(in)

    assert(res.sameElements(resU))
  }

  // TODO: Slow test, takes a very long time due to the problems with higher-kinded types
  ignore("zipCollectionWithIndex3_Free")  {
    class ScalanStateF extends ScalanCommunityDslExp with StateExamples with MonadsDslExp {
      val State = new FreeStateManager[Int]
    }

    val progStaged = new CommunityLmsCompilerScala(new ScalanStateF) with CoreBridge with EffectfulCompiler[ScalanStateF]

    // TODO move into a separate ScalanStateFItTests?
    val res = getStagedOutput(progStaged)((_: ScalanStateF).zipCollectionWithIndexW3, "zipCollectionWithIndex3_Free", in)
  }
}

trait IfBranchesProg extends ScalanCommunityDsl {
  lazy val t1 = fun { (in: Rep[String]) =>
    IF (in.contains("abc")) THEN { console_printlnE(in) } ELSE { console_printlnE(in) }
  }
  lazy val t2 = fun { (in: Rep[String]) =>
    val input = console_readlineE()
    val user = IF (input !== (null: String)) { input } ELSE { "admin" }
    IF (in.contains(user)) THEN {
      console_printlnE(in)
    } ELSE {
      console_printlnE(in + "rejected")
    }
  }
}

class IfBranchesItTests extends BaseItTests[IfBranchesProg](new ScalanCommunityDslSeq with IfBranchesProg) {

  val progStaged = new CommunityLmsCompilerScala(new ScalanCommunityDslExp with IfBranchesProg) with CoreBridge with EffectfulCompiler[ScalanCommunityDslExp with IfBranchesProg]
  val defaultCompilers = compilers(progStaged)

  test("ifBranches")  {
    //pending
    val in = "abc"
    //val actual = getStagedOutput(_.t1)(in)
    val actual2 = getStagedOutput(_.t2)(in)
  }
}

trait CrossDomainProg extends ScalanCommunityDsl with CrossDomainExample

class CrossDomainItTests extends BaseItTests[CrossDomainProg](new ScalanCommunityDslSeq with CrossDomainProg with InteractionsDslSeq with AuthenticationsDslSeq) {

  class EffectsExp extends ScalanCommunityDslExp with CrossDomainProg with InteractionsDslExp with AuthenticationsDslExp with JNIExtractorOpsExp

  val progStaged = new CommunityLmsCompilerScala(new EffectsExp) with CoreBridge with EffectfulCompiler[EffectsExp]

  val defaultCompilers = compilers(progStaged)

  // TODO: Slow test
  ignore("runCrossDomain")  {
    val in = 10
    val actual = getStagedOutput(_.runAppW)(in)
  }
}

class EffectsJniItTests extends BaseItTests[State0Prog](???) {

  class EffectsExpCxx extends ScalanCommunityDslExp with JNIExtractorOpsExp with MonadsDslExp with State0Prog {
    override val State = new State0Manager[Int]

    lazy val jniZipArrayWithIndexW = JNI_Wrap(zipArrayWithIndexW)

    lazy val jniZipCollectionWithIndexW = JNI_Wrap(zipCollectionWithIndexW)

    lazy val jniZipCollectionWithIndexW2 = JNI_Wrap(zipCollectionWithIndexW2)

    lazy val jniZipCollectionWithIndexW3 = JNI_Wrap(zipCollectionWithIndexW3)
  }
  val progcxx = new LmsCompilerCxx(new EffectsExpCxx) with CoreBridge with JNIBridge with EffectfulCompiler[EffectsExpCxx]

  val defaultCompilers = compilers(progcxx)

  // FIXME temporary workaround for compileSource below not compiling
  def generate[A, B](back: Compiler[_ <: ScalanCtxExp])(f: back.Exp[A => B], functionName: String)
                    (implicit config: back.CompilerConfig): Unit = {
    val dir = new File(prefix, functionName)
    back.buildExecutable(dir, dir, functionName, f, GraphVizConfig.default)
  }

  test("jniZipArrayWithIndex") {
    // compileSource(_.jniZipArrayWithIndex,"jniZipArrayWithIndex")
    generate(progcxx)(progcxx.scalan.jniZipArrayWithIndexW,"jniZipArrayWithIndex")(progcxx.defaultCompilerConfig)
  }
  test("jniZipCollectionWithIndex") {
    generate(progcxx)(progcxx.scalan.jniZipCollectionWithIndexW,"jniZipCollectionWithIndex")(progcxx.defaultCompilerConfig)
  }
  test("jniZipCollectionWithIndex2") {
    generate(progcxx)(progcxx.scalan.jniZipCollectionWithIndexW2,"jniZipCollectionWithIndex2")(progcxx.defaultCompilerConfig)
  }
  test("jniZipCollectionWithIndex3") {
    generate(progcxx)(progcxx.scalan.jniZipCollectionWithIndexW3,"jniZipCollectionWithIndex3")(progcxx.defaultCompilerConfig)
  }
}
