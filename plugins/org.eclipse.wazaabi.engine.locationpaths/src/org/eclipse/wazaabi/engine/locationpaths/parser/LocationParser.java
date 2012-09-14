/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.locationpaths.parser;
import java.util.ArrayList;
import org.eclipse.wazaabi.engine.locationpaths.model.Axis;
import org.eclipse.wazaabi.engine.locationpaths.model.Expression;
import org.eclipse.wazaabi.engine.locationpaths.model.BinaryExpression;
import org.eclipse.wazaabi.engine.locationpaths.model.EqualityExpression;
import org.eclipse.wazaabi.engine.locationpaths.model.Operator;
import org.eclipse.wazaabi.engine.locationpaths.model.IntegerExpression;
import org.eclipse.wazaabi.engine.locationpaths.model.LiteralExpression;
import org.eclipse.wazaabi.engine.locationpaths.model.LocationPath;
import org.eclipse.wazaabi.engine.locationpaths.model.Step;
import org.eclipse.wazaabi.engine.locationpaths.model.InitialContext;
import org.eclipse.wazaabi.engine.locationpaths.model.QueryContext;

public class LocationParser implements LocationParserConstants {

  final public LocationPath LocationPath() throws ParseException {
  LocationPath locationPath = new LocationPath();
  InitialContext initialContext = null;
    if (jj_2_1(2)) {
      initialContext = InitialContext();
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case SLASH:
        jj_consume_token(SLASH);
        RelativeLocationPath(locationPath);
        break;
      default:
        jj_la1[0] = jj_gen;
        ;
      }
    } else {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case 1:
      case 4:
      case 5:
      case 6:
      case 7:
      case 8:
      case AXIS_SELF:
      case AXIS_CHILD:
      case AXIS_ATTRIBUTE:
      case AXIS_REFERENCE:
      case AXIS_VARIABLE:
      case AXIS_DESCENDANT_OR_SELF:
      case AXIS_PARENT:
      case AXIS_CLASS:
      case AXIS_PACKAGE:
      case NODE:
      case IDENTIFIER:
        RelativeLocationPath(locationPath);
        break;
      default:
        jj_la1[1] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
    }
    if (initialContext != null) locationPath.setInitialContext(initialContext);
    {if (true) return locationPath;}
    throw new Error("Missing return statement in function");
  }

  final public LocationPath RelativeLocationPath(LocationPath locationPath) throws ParseException {
    NodeTest(locationPath);
    label_1:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case SLASH:
      case SLASHSLASH:
        ;
        break;
      default:
        jj_la1[2] = jj_gen;
        break label_1;
      }
      LocationStep(locationPath);
    }
    {if (true) return locationPath;}
    throw new Error("Missing return statement in function");
  }

  final public InitialContext InitialContext() throws ParseException {
  InitialContext initialContext = null;
    initialContext = QueryContext();
    {if (true) return initialContext;}
    throw new Error("Missing return statement in function");
  }

  final public void LocationStep(LocationPath locationPath) throws ParseException {
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case SLASH:
      jj_consume_token(SLASH);
      break;
    case SLASHSLASH:
      jj_consume_token(SLASHSLASH);
      Step step = new Step();
      step.setAxis(Axis.DESCENDANT_OR_SELF);
      step.setNameTest(null);
      locationPath.getSteps().add(step);
      break;
    default:
      jj_la1[3] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
    NodeTest(locationPath);
  }

  final public String WildcardName() throws ParseException {
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case 1:
      jj_consume_token(1);
      break;
    case IDENTIFIER:
      jj_consume_token(IDENTIFIER);
      break;
    default:
      jj_la1[4] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
    {if (true) return (String) token.image;}

    throw new Error("Missing return statement in function");
  }

  final public void NodeTest(LocationPath locationPath) throws ParseException {
  int axis;
  int type =-1;
  String name = null;
  Expression p;
  Step step = new Step();
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case 1:
    case 6:
    case 7:
    case 8:
    case AXIS_SELF:
    case AXIS_CHILD:
    case AXIS_ATTRIBUTE:
    case AXIS_REFERENCE:
    case AXIS_VARIABLE:
    case AXIS_DESCENDANT_OR_SELF:
    case AXIS_PARENT:
    case AXIS_CLASS:
    case AXIS_PACKAGE:
    case NODE:
    case IDENTIFIER:
      axis = AxisSpecifier();
      if (jj_2_2(2147483647)) {
        type = NodeType();
        jj_consume_token(2);
        jj_consume_token(3);
      } else {
        switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
        case 1:
        case IDENTIFIER:
          name = WildcardName();
          break;
        default:
          jj_la1[5] = jj_gen;
          jj_consume_token(-1);
          throw new ParseException();
        }
      }
      break;
    case 4:
      jj_consume_token(4);
        axis = Axis.SELF;
      break;
    case 5:
      jj_consume_token(5);
        axis = Axis.PARENT;
      break;
    default:
      jj_la1[6] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
    label_2:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case 9:
        ;
        break;
      default:
        jj_la1[7] = jj_gen;
        break label_2;
      }
      p = Predicate();
        step.getPredicates().add(p);
    }
    step.setAxis(axis);
    if (name != null) step.setNameTest(name);
    locationPath.getSteps().add(step);
  }

  final public int NodeType() throws ParseException {
  int type;
    jj_consume_token(NODE);
    type = 1;
    {if (true) return type;}
    throw new Error("Missing return statement in function");
  }

  final public int AxisSpecifier() throws ParseException {
  int axis;
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case AXIS_SELF:
    case AXIS_CHILD:
    case AXIS_ATTRIBUTE:
    case AXIS_REFERENCE:
    case AXIS_VARIABLE:
    case AXIS_DESCENDANT_OR_SELF:
    case AXIS_PARENT:
    case AXIS_CLASS:
    case AXIS_PACKAGE:
      axis = AxisName();
      break;
    default:
      jj_la1[8] = jj_gen;
      axis = AbbreviatedAxisSpecifier();
    }
    {if (true) return axis;}
    throw new Error("Missing return statement in function");
  }

  final public int AbbreviatedAxisSpecifier() throws ParseException {
  int axis = Axis.CHILD;
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case 6:
    case 7:
    case 8:
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case 6:
        jj_consume_token(6);
        axis = Axis.ATTRIBUTE;
        break;
      case 7:
        jj_consume_token(7);
        axis = Axis.REFERENCE;
        break;
      case 8:
        jj_consume_token(8);
        axis = Axis.VARIABLE;
        break;
      default:
        jj_la1[9] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
      break;
    default:
      jj_la1[10] = jj_gen;
      ;
    }
    {if (true) return axis;}
    throw new Error("Missing return statement in function");
  }

  final public int AxisName() throws ParseException {
  int axis;
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case AXIS_SELF:
      jj_consume_token(AXIS_SELF);
      axis = Axis.SELF;
      break;
    case AXIS_CHILD:
      jj_consume_token(AXIS_CHILD);
      axis = Axis.CHILD;
      break;
    case AXIS_ATTRIBUTE:
      jj_consume_token(AXIS_ATTRIBUTE);
      axis = Axis.ATTRIBUTE;
      break;
    case AXIS_REFERENCE:
      jj_consume_token(AXIS_REFERENCE);
      axis = Axis.REFERENCE;
      break;
    case AXIS_VARIABLE:
      jj_consume_token(AXIS_VARIABLE);
      axis = Axis.VARIABLE;
      break;
    case AXIS_DESCENDANT_OR_SELF:
      jj_consume_token(AXIS_DESCENDANT_OR_SELF);
      axis = Axis.DESCENDANT_OR_SELF;
      break;
    case AXIS_PARENT:
      jj_consume_token(AXIS_PARENT);
      axis = Axis.PARENT;
      break;
    case AXIS_CLASS:
      jj_consume_token(AXIS_CLASS);
      axis = Axis.CLASS;
      break;
    case AXIS_PACKAGE:
      jj_consume_token(AXIS_PACKAGE);
      axis = Axis.PACKAGE;
      break;
    default:
      jj_la1[11] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
    {if (true) return axis;}
    throw new Error("Missing return statement in function");
  }

  final public Expression Predicate() throws ParseException {
  Expression ex = null;
    jj_consume_token(9);
    ex = Expression();
    jj_consume_token(10);
    {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

  final public Expression Expression() throws ParseException {
  Expression ex;
    ex = OrExpr();
    {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

  final public Expression PrimaryExpr() throws ParseException {
  Expression ex = null;
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case Literal:
      jj_consume_token(Literal);
      ex = new LiteralExpression();
      ((LiteralExpression) ex).setValue(token.image.substring(1, token.image.length() - 1));
      break;
    case Number:
      jj_consume_token(Number);
      ex = new IntegerExpression();
      ((IntegerExpression) ex).setValue(Integer.parseInt(token.image));
      break;
    default:
      jj_la1[12] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
    {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

  final public Expression FilterExpr() throws ParseException {
  Expression ex, p;
  ArrayList ps = new ArrayList();
  boolean path = false;
  LocationPath locationPath = new LocationPath();
  ArrayList steps = new ArrayList();
    ex = PrimaryExpr();
    label_3:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case 9:
        ;
        break;
      default:
        jj_la1[13] = jj_gen;
        break label_3;
      }
      p = Predicate();
        path = true;
        ps.add(p);
    }
    label_4:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case SLASH:
      case SLASHSLASH:
        ;
        break;
      default:
        jj_la1[14] = jj_gen;
        break label_4;
      }
      LocationStep(locationPath);
        path = true;
    }
    if (path)
    { /*return compiler.expressionPath(ex, ps.toArray(), steps.toArray());*/}
    else
    {
      {if (true) return ex;}
    }
    throw new Error("Missing return statement in function");
  }

  final public Expression OrExpr() throws ParseException {
  Expression ex, r = null;
    ex = AndExpr();
    label_5:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case OR:
        ;
        break;
      default:
        jj_la1[15] = jj_gen;
        break label_5;
      }
      jj_consume_token(OR);
      r = AndExpr();
    }
    if (r == null) {if (true) return ex;}
    else
    {
      {if (true) return new BinaryExpression(ex, r, Operator.OR);}
    }
    throw new Error("Missing return statement in function");
  }

  final public Expression AndExpr() throws ParseException {
  Expression ex, r = null;
    ex = EqualityExpr();
    label_6:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case AND:
        ;
        break;
      default:
        jj_la1[16] = jj_gen;
        break label_6;
      }
      jj_consume_token(AND);
      r = EqualityExpr();
    }
    if (r == null) {if (true) return ex;}
    else
    {
      {if (true) return new BinaryExpression(ex, r, Operator.AND);}
    }
    throw new Error("Missing return statement in function");
  }

  final public Expression EqualityExpr() throws ParseException {
  Expression ex, r, tmp;
    ex = PathExpr();
    label_7:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case EQ:
      case NEQ:
        ;
        break;
      default:
        jj_la1[17] = jj_gen;
        break label_7;
      }
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case EQ:
        jj_consume_token(EQ);
        r = PathExpr();
          ex = new EqualityExpression(ex, r, Operator.EQUAL);
        break;
      case NEQ:
        jj_consume_token(NEQ);
        r = PathExpr();
          ex = new EqualityExpression(ex, r, Operator.NOT_EQUAL);
        break;
      default:
        jj_la1[18] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
    }
    {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

  final public Expression PathExpr() throws ParseException {
  Expression ex = null;
  Object [ ] steps;
    if (jj_2_3(2147483647)) {
      ex = FilterExpr();
    } else {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case 1:
      case 4:
      case 5:
      case 6:
      case 7:
      case 8:
      case AXIS_SELF:
      case AXIS_CHILD:
      case AXIS_ATTRIBUTE:
      case AXIS_REFERENCE:
      case AXIS_VARIABLE:
      case AXIS_DESCENDANT_OR_SELF:
      case AXIS_PARENT:
      case AXIS_CLASS:
      case AXIS_PACKAGE:
      case NODE:
      case OR:
      case AND:
      case MOD:
      case DIV:
      case IDENTIFIER:
        ex = LocationPath();
        break;
      default:
        jj_la1[19] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
    }
    {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

  final public QueryContext QueryContext() throws ParseException {
  String name;
  ArrayList args;
  QueryContext queryContext = new QueryContext();
    name = FunctionName();
    args = ArgumentList();
    if (args != null)
    {
      queryContext.setArgs(args);
    }
    queryContext.setFunctionName(name);
    {if (true) return queryContext;}
    throw new Error("Missing return statement in function");
  }

  final public String FunctionName() throws ParseException {
  String qname;
    qname = Identifier_Without_CoreFunctions();
    {if (true) return qname;}
    throw new Error("Missing return statement in function");
  }

  final public String Identifier() throws ParseException {
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case OR:
    case AND:
    case MOD:
    case DIV:
    case IDENTIFIER:
      Identifier_Without_CoreFunctions();
      break;
    case NODE:
      jj_consume_token(NODE);
      break;
    default:
      jj_la1[20] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
    {if (true) return token.image;}
    throw new Error("Missing return statement in function");
  }

  final public String Identifier_Without_CoreFunctions() throws ParseException {
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case IDENTIFIER:
      jj_consume_token(IDENTIFIER);
      break;
    case OR:
      jj_consume_token(OR);
      break;
    case AND:
      jj_consume_token(AND);
      break;
    case MOD:
      jj_consume_token(MOD);
      break;
    case DIV:
      jj_consume_token(DIV);
      break;
    default:
      jj_la1[21] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
    {if (true) return token.image;}
    throw new Error("Missing return statement in function");
  }

  final public ArrayList ArgumentList() throws ParseException {
  ArrayList args = null;
  Object arg;
    jj_consume_token(2);
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case 1:
    case 4:
    case 5:
    case 6:
    case 7:
    case 8:
    case AXIS_SELF:
    case AXIS_CHILD:
    case AXIS_ATTRIBUTE:
    case AXIS_REFERENCE:
    case AXIS_VARIABLE:
    case AXIS_DESCENDANT_OR_SELF:
    case AXIS_PARENT:
    case AXIS_CLASS:
    case AXIS_PACKAGE:
    case NODE:
    case OR:
    case AND:
    case MOD:
    case DIV:
    case IDENTIFIER:
    case Literal:
    case Number:
      arg = Argument();
      args = new ArrayList();
      args.add(arg);
      label_8:
      while (true) {
        switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
        case 11:
          ;
          break;
        default:
          jj_la1[22] = jj_gen;
          break label_8;
        }
        jj_consume_token(11);
        arg = Argument();
        args.add(arg);
      }
      break;
    default:
      jj_la1[23] = jj_gen;
      ;
    }
    jj_consume_token(3);
    {if (true) return args;}
    throw new Error("Missing return statement in function");
  }

  final public Object Argument() throws ParseException {
  Object ex;
    ex = Expression();
    {if (true) return ex;}
    throw new Error("Missing return statement in function");
  }

  private boolean jj_2_1(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_1(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(0, xla); }
  }

  private boolean jj_2_2(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_2(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(1, xla); }
  }

  private boolean jj_2_3(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_3(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(2, xla); }
  }

  private boolean jj_3R_14() {
    if (jj_scan_token(Number)) return true;
    return false;
  }

  private boolean jj_3R_12() {
    if (jj_3R_15()) return true;
    if (jj_3R_16()) return true;
    return false;
  }

  private boolean jj_3_3() {
    if (jj_3R_11()) return true;
    return false;
  }

  private boolean jj_3R_13() {
    if (jj_scan_token(Literal)) return true;
    return false;
  }

  private boolean jj_3R_15() {
    if (jj_3R_17()) return true;
    return false;
  }

  private boolean jj_3R_17() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_scan_token(31)) {
    jj_scanpos = xsp;
    if (jj_scan_token(27)) {
    jj_scanpos = xsp;
    if (jj_scan_token(28)) {
    jj_scanpos = xsp;
    if (jj_scan_token(29)) {
    jj_scanpos = xsp;
    if (jj_scan_token(30)) return true;
    }
    }
    }
    }
    return false;
  }

  private boolean jj_3R_11() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_13()) {
    jj_scanpos = xsp;
    if (jj_3R_14()) return true;
    }
    return false;
  }

  private boolean jj_3R_9() {
    if (jj_3R_12()) return true;
    return false;
  }

  private boolean jj_3_2() {
    if (jj_3R_10()) return true;
    if (jj_scan_token(2)) return true;
    if (jj_scan_token(3)) return true;
    return false;
  }

  private boolean jj_3R_16() {
    if (jj_scan_token(2)) return true;
    return false;
  }

  private boolean jj_3_1() {
    if (jj_3R_9()) return true;
    return false;
  }

  private boolean jj_3R_10() {
    if (jj_scan_token(NODE)) return true;
    return false;
  }

  /** Generated Token Manager. */
  public LocationParserTokenManager token_source;
  JavaCharStream jj_input_stream;
  /** Current token. */
  public Token token;
  /** Next token. */
  public Token jj_nt;
  private int jj_ntk;
  private Token jj_scanpos, jj_lastpos;
  private int jj_la;
  private int jj_gen;
  final private int[] jj_la1 = new int[24];
  static private int[] jj_la1_0;
  static private int[] jj_la1_1;
  static {
      jj_la1_init_0();
      jj_la1_init_1();
   }
   private static void jj_la1_init_0() {
      jj_la1_0 = new int[] {0x0,0x87fe01f2,0x0,0x0,0x80000002,0x80000002,0x87fe01f2,0x200,0x3fe0000,0x1c0,0x1c0,0x3fe0000,0x0,0x200,0x0,0x8000000,0x10000000,0x0,0x0,0xfffe01f2,0xfc000000,0xf8000000,0x800,0xfffe01f2,};
   }
   private static void jj_la1_init_1() {
      jj_la1_1 = new int[] {0x4,0x0,0xc,0xc,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x140,0x0,0xc,0x0,0x0,0x30,0x30,0x0,0x0,0x0,0x0,0x140,};
   }
  final private JJCalls[] jj_2_rtns = new JJCalls[3];
  private boolean jj_rescan = false;
  private int jj_gc = 0;

  /** Constructor with InputStream. */
  public LocationParser(java.io.InputStream stream) {
     this(stream, null);
  }
  /** Constructor with InputStream and supplied encoding */
  public LocationParser(java.io.InputStream stream, String encoding) {
    try { jj_input_stream = new JavaCharStream(stream, encoding, 1, 1); } catch(java.io.UnsupportedEncodingException e) { throw new RuntimeException(e); }
    token_source = new LocationParserTokenManager(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 24; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  /** Reinitialise. */
  public void ReInit(java.io.InputStream stream) {
     ReInit(stream, null);
  }
  /** Reinitialise. */
  public void ReInit(java.io.InputStream stream, String encoding) {
    try { jj_input_stream.ReInit(stream, encoding, 1, 1); } catch(java.io.UnsupportedEncodingException e) { throw new RuntimeException(e); }
    token_source.ReInit(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 24; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  /** Constructor. */
  public LocationParser(java.io.Reader stream) {
    jj_input_stream = new JavaCharStream(stream, 1, 1);
    token_source = new LocationParserTokenManager(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 24; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  /** Reinitialise. */
  public void ReInit(java.io.Reader stream) {
    jj_input_stream.ReInit(stream, 1, 1);
    token_source.ReInit(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 24; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  /** Constructor with generated Token Manager. */
  public LocationParser(LocationParserTokenManager tm) {
    token_source = tm;
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 24; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  /** Reinitialise. */
  public void ReInit(LocationParserTokenManager tm) {
    token_source = tm;
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 24; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  private Token jj_consume_token(int kind) throws ParseException {
    Token oldToken;
    if ((oldToken = token).next != null) token = token.next;
    else token = token.next = token_source.getNextToken();
    jj_ntk = -1;
    if (token.kind == kind) {
      jj_gen++;
      if (++jj_gc > 100) {
        jj_gc = 0;
        for (int i = 0; i < jj_2_rtns.length; i++) {
          JJCalls c = jj_2_rtns[i];
          while (c != null) {
            if (c.gen < jj_gen) c.first = null;
            c = c.next;
          }
        }
      }
      return token;
    }
    token = oldToken;
    jj_kind = kind;
    throw generateParseException();
  }

  static private final class LookaheadSuccess extends java.lang.Error { }
  final private LookaheadSuccess jj_ls = new LookaheadSuccess();
  private boolean jj_scan_token(int kind) {
    if (jj_scanpos == jj_lastpos) {
      jj_la--;
      if (jj_scanpos.next == null) {
        jj_lastpos = jj_scanpos = jj_scanpos.next = token_source.getNextToken();
      } else {
        jj_lastpos = jj_scanpos = jj_scanpos.next;
      }
    } else {
      jj_scanpos = jj_scanpos.next;
    }
    if (jj_rescan) {
      int i = 0; Token tok = token;
      while (tok != null && tok != jj_scanpos) { i++; tok = tok.next; }
      if (tok != null) jj_add_error_token(kind, i);
    }
    if (jj_scanpos.kind != kind) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) throw jj_ls;
    return false;
  }


/** Get the next Token. */
  final public Token getNextToken() {
    if (token.next != null) token = token.next;
    else token = token.next = token_source.getNextToken();
    jj_ntk = -1;
    jj_gen++;
    return token;
  }

/** Get the specific Token. */
  final public Token getToken(int index) {
    Token t = token;
    for (int i = 0; i < index; i++) {
      if (t.next != null) t = t.next;
      else t = t.next = token_source.getNextToken();
    }
    return t;
  }

  private int jj_ntk() {
    if ((jj_nt=token.next) == null)
      return (jj_ntk = (token.next=token_source.getNextToken()).kind);
    else
      return (jj_ntk = jj_nt.kind);
  }

  private java.util.List jj_expentries = new java.util.ArrayList();
  private int[] jj_expentry;
  private int jj_kind = -1;
  private int[] jj_lasttokens = new int[100];
  private int jj_endpos;

  private void jj_add_error_token(int kind, int pos) {
    if (pos >= 100) return;
    if (pos == jj_endpos + 1) {
      jj_lasttokens[jj_endpos++] = kind;
    } else if (jj_endpos != 0) {
      jj_expentry = new int[jj_endpos];
      for (int i = 0; i < jj_endpos; i++) {
        jj_expentry[i] = jj_lasttokens[i];
      }
      jj_entries_loop: for (java.util.Iterator it = jj_expentries.iterator(); it.hasNext();) {
        int[] oldentry = (int[])(it.next());
        if (oldentry.length == jj_expentry.length) {
          for (int i = 0; i < jj_expentry.length; i++) {
            if (oldentry[i] != jj_expentry[i]) {
              continue jj_entries_loop;
            }
          }
          jj_expentries.add(jj_expentry);
          break jj_entries_loop;
        }
      }
      if (pos != 0) jj_lasttokens[(jj_endpos = pos) - 1] = kind;
    }
  }

  /** Generate ParseException. */
  public ParseException generateParseException() {
    jj_expentries.clear();
    boolean[] la1tokens = new boolean[41];
    if (jj_kind >= 0) {
      la1tokens[jj_kind] = true;
      jj_kind = -1;
    }
    for (int i = 0; i < 24; i++) {
      if (jj_la1[i] == jj_gen) {
        for (int j = 0; j < 32; j++) {
          if ((jj_la1_0[i] & (1<<j)) != 0) {
            la1tokens[j] = true;
          }
          if ((jj_la1_1[i] & (1<<j)) != 0) {
            la1tokens[32+j] = true;
          }
        }
      }
    }
    for (int i = 0; i < 41; i++) {
      if (la1tokens[i]) {
        jj_expentry = new int[1];
        jj_expentry[0] = i;
        jj_expentries.add(jj_expentry);
      }
    }
    jj_endpos = 0;
    jj_rescan_token();
    jj_add_error_token(0, 0);
    int[][] exptokseq = new int[jj_expentries.size()][];
    for (int i = 0; i < jj_expentries.size(); i++) {
      exptokseq[i] = (int[])jj_expentries.get(i);
    }
    return new ParseException(token, exptokseq, tokenImage);
  }

  /** Enable tracing. */
  final public void enable_tracing() {
  }

  /** Disable tracing. */
  final public void disable_tracing() {
  }

  private void jj_rescan_token() {
    jj_rescan = true;
    for (int i = 0; i < 3; i++) {
    try {
      JJCalls p = jj_2_rtns[i];
      do {
        if (p.gen > jj_gen) {
          jj_la = p.arg; jj_lastpos = jj_scanpos = p.first;
          switch (i) {
            case 0: jj_3_1(); break;
            case 1: jj_3_2(); break;
            case 2: jj_3_3(); break;
          }
        }
        p = p.next;
      } while (p != null);
      } catch(LookaheadSuccess ls) { }
    }
    jj_rescan = false;
  }

  private void jj_save(int index, int xla) {
    JJCalls p = jj_2_rtns[index];
    while (p.gen > jj_gen) {
      if (p.next == null) { p = p.next = new JJCalls(); break; }
      p = p.next;
    }
    p.gen = jj_gen + xla - jj_la; p.first = token; p.arg = xla;
  }

  static final class JJCalls {
    int gen;
    Token first;
    int arg;
    JJCalls next;
  }

 }
