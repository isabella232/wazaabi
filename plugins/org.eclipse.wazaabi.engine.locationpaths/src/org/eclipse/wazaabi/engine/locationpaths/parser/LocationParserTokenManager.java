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

/** Token Manager. */
public class LocationParserTokenManager implements LocationParserConstants
{

  /** Debug output. */
  public  java.io.PrintStream debugStream = System.out;
  /** Set debug output. */
  public  void setDebugStream(java.io.PrintStream ds) { debugStream = ds; }
private final int jjStopStringLiteralDfa_0(int pos, long active0)
{
   switch (pos)
   {
      case 0:
         if ((active0 & 0x7ffe0000L) != 0L)
         {
            jjmatchedKind = 31;
            return 1;
         }
         return -1;
      case 1:
         if ((active0 & 0x8000000L) != 0L)
            return 1;
         if ((active0 & 0x77fe0000L) != 0L)
         {
            jjmatchedKind = 31;
            jjmatchedPos = 1;
            return 1;
         }
         return -1;
      case 2:
         if ((active0 & 0x70000000L) != 0L)
            return 1;
         if ((active0 & 0x7fe0000L) != 0L)
         {
            jjmatchedKind = 31;
            jjmatchedPos = 2;
            return 1;
         }
         return -1;
      case 3:
         if ((active0 & 0x4000000L) != 0L)
            return 1;
         if ((active0 & 0x3fe0000L) != 0L)
         {
            jjmatchedKind = 31;
            jjmatchedPos = 3;
            return 1;
         }
         return -1;
      case 4:
         if ((active0 & 0x3fc0000L) != 0L)
         {
            jjmatchedKind = 31;
            jjmatchedPos = 4;
            return 1;
         }
         if ((active0 & 0x20000L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 3;
            }
            return -1;
         }
         return -1;
      case 5:
         if ((active0 & 0x2f80000L) != 0L)
         {
            jjmatchedKind = 31;
            jjmatchedPos = 5;
            return 1;
         }
         if ((active0 & 0x1040000L) != 0L)
         {
            if (jjmatchedPos < 4)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 4;
            }
            return -1;
         }
         if ((active0 & 0x20000L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 3;
            }
            return -1;
         }
         return -1;
      case 6:
         if ((active0 & 0x2780000L) != 0L)
         {
            jjmatchedKind = 31;
            jjmatchedPos = 6;
            return 1;
         }
         if ((active0 & 0x800000L) != 0L)
         {
            if (jjmatchedPos < 5)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 5;
            }
            return -1;
         }
         if ((active0 & 0x1040000L) != 0L)
         {
            if (jjmatchedPos < 4)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 4;
            }
            return -1;
         }
         return -1;
      case 7:
         if ((active0 & 0x780000L) != 0L)
         {
            jjmatchedKind = 31;
            jjmatchedPos = 7;
            return 1;
         }
         if ((active0 & 0x2000000L) != 0L)
         {
            if (jjmatchedPos < 6)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 6;
            }
            return -1;
         }
         if ((active0 & 0x800000L) != 0L)
         {
            if (jjmatchedPos < 5)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 5;
            }
            return -1;
         }
         return -1;
      case 8:
         if ((active0 & 0x580000L) != 0L)
         {
            jjmatchedKind = 31;
            jjmatchedPos = 8;
            return 1;
         }
         if ((active0 & 0x2000000L) != 0L)
         {
            if (jjmatchedPos < 6)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 6;
            }
            return -1;
         }
         if ((active0 & 0x200000L) != 0L)
         {
            if (jjmatchedPos < 7)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 7;
            }
            return -1;
         }
         return -1;
      case 9:
         if ((active0 & 0x400000L) != 0L)
         {
            jjmatchedKind = 31;
            jjmatchedPos = 9;
            return 1;
         }
         if ((active0 & 0x200000L) != 0L)
         {
            if (jjmatchedPos < 7)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 7;
            }
            return -1;
         }
         if ((active0 & 0x180000L) != 0L)
         {
            if (jjmatchedPos < 8)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 8;
            }
            return -1;
         }
         return -1;
      case 10:
         if ((active0 & 0x400000L) != 0L)
         {
            if (jjmatchedPos < 9)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 9;
            }
            return -1;
         }
         if ((active0 & 0x180000L) != 0L)
         {
            if (jjmatchedPos < 8)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 8;
            }
            return -1;
         }
         return -1;
      case 11:
         if ((active0 & 0x400000L) != 0L)
         {
            if (jjmatchedPos < 9)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 9;
            }
            return -1;
         }
         return -1;
      case 12:
         if ((active0 & 0x400000L) != 0L)
         {
            if (jjmatchedPos < 9)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 9;
            }
            return -1;
         }
         return -1;
      case 13:
         if ((active0 & 0x400000L) != 0L)
         {
            if (jjmatchedPos < 9)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 9;
            }
            return -1;
         }
         return -1;
      case 14:
         if ((active0 & 0x400000L) != 0L)
         {
            if (jjmatchedPos < 9)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 9;
            }
            return -1;
         }
         return -1;
      case 15:
         if ((active0 & 0x400000L) != 0L)
         {
            if (jjmatchedPos < 9)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 9;
            }
            return -1;
         }
         return -1;
      case 16:
         if ((active0 & 0x400000L) != 0L)
         {
            if (jjmatchedPos < 9)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 9;
            }
            return -1;
         }
         return -1;
      case 17:
         if ((active0 & 0x400000L) != 0L)
         {
            if (jjmatchedPos < 9)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 9;
            }
            return -1;
         }
         return -1;
      case 18:
         if ((active0 & 0x400000L) != 0L)
         {
            if (jjmatchedPos < 9)
            {
               jjmatchedKind = 31;
               jjmatchedPos = 9;
            }
            return -1;
         }
         return -1;
      default :
         return -1;
   }
}
private final int jjStartNfa_0(int pos, long active0)
{
   return jjMoveNfa_0(jjStopStringLiteralDfa_0(pos, active0), pos + 1);
}
private int jjStopAtPos(int pos, int kind)
{
   jjmatchedKind = kind;
   jjmatchedPos = pos;
   return pos + 1;
}
private int jjMoveStringLiteralDfa0_0()
{
   switch(curChar)
   {
      case 33:
         return jjMoveStringLiteralDfa1_0(0x2000000000L);
      case 36:
         return jjStopAtPos(0, 8);
      case 38:
         return jjStopAtPos(0, 7);
      case 40:
         return jjStopAtPos(0, 2);
      case 41:
         return jjStopAtPos(0, 3);
      case 42:
         return jjStopAtPos(0, 1);
      case 44:
         return jjStopAtPos(0, 11);
      case 46:
         jjmatchedKind = 4;
         return jjMoveStringLiteralDfa1_0(0x20L);
      case 47:
         jjmatchedKind = 34;
         return jjMoveStringLiteralDfa1_0(0x800000000L);
      case 61:
         return jjStopAtPos(0, 36);
      case 64:
         return jjStopAtPos(0, 6);
      case 91:
         return jjStopAtPos(0, 9);
      case 93:
         return jjStopAtPos(0, 10);
      case 97:
         return jjMoveStringLiteralDfa1_0(0x10080000L);
      case 99:
         return jjMoveStringLiteralDfa1_0(0x1040000L);
      case 100:
         return jjMoveStringLiteralDfa1_0(0x40400000L);
      case 109:
         return jjMoveStringLiteralDfa1_0(0x20000000L);
      case 110:
         return jjMoveStringLiteralDfa1_0(0x4000000L);
      case 111:
         return jjMoveStringLiteralDfa1_0(0x8000000L);
      case 112:
         return jjMoveStringLiteralDfa1_0(0x2800000L);
      case 114:
         return jjMoveStringLiteralDfa1_0(0x100000L);
      case 115:
         return jjMoveStringLiteralDfa1_0(0x20000L);
      case 118:
         return jjMoveStringLiteralDfa1_0(0x200000L);
      default :
         return jjMoveNfa_0(0, 0);
   }
}
private int jjMoveStringLiteralDfa1_0(long active0)
{
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(0, active0);
      return 1;
   }
   switch(curChar)
   {
      case 46:
         if ((active0 & 0x20L) != 0L)
            return jjStopAtPos(1, 5);
         break;
      case 47:
         if ((active0 & 0x800000000L) != 0L)
            return jjStopAtPos(1, 35);
         break;
      case 61:
         if ((active0 & 0x2000000000L) != 0L)
            return jjStopAtPos(1, 37);
         break;
      case 97:
         return jjMoveStringLiteralDfa2_0(active0, 0x2a00000L);
      case 101:
         return jjMoveStringLiteralDfa2_0(active0, 0x520000L);
      case 104:
         return jjMoveStringLiteralDfa2_0(active0, 0x40000L);
      case 105:
         return jjMoveStringLiteralDfa2_0(active0, 0x40000000L);
      case 108:
         return jjMoveStringLiteralDfa2_0(active0, 0x1000000L);
      case 110:
         return jjMoveStringLiteralDfa2_0(active0, 0x10000000L);
      case 111:
         return jjMoveStringLiteralDfa2_0(active0, 0x24000000L);
      case 114:
         if ((active0 & 0x8000000L) != 0L)
            return jjStartNfaWithStates_0(1, 27, 1);
         break;
      case 116:
         return jjMoveStringLiteralDfa2_0(active0, 0x80000L);
      default :
         break;
   }
   return jjStartNfa_0(0, active0);
}
private int jjMoveStringLiteralDfa2_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(0, old0);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(1, active0);
      return 2;
   }
   switch(curChar)
   {
      case 97:
         return jjMoveStringLiteralDfa3_0(active0, 0x1000000L);
      case 99:
         return jjMoveStringLiteralDfa3_0(active0, 0x2000000L);
      case 100:
         if ((active0 & 0x10000000L) != 0L)
            return jjStartNfaWithStates_0(2, 28, 1);
         else if ((active0 & 0x20000000L) != 0L)
            return jjStartNfaWithStates_0(2, 29, 1);
         return jjMoveStringLiteralDfa3_0(active0, 0x4000000L);
      case 102:
         return jjMoveStringLiteralDfa3_0(active0, 0x100000L);
      case 105:
         return jjMoveStringLiteralDfa3_0(active0, 0x40000L);
      case 108:
         return jjMoveStringLiteralDfa3_0(active0, 0x20000L);
      case 114:
         return jjMoveStringLiteralDfa3_0(active0, 0xa00000L);
      case 115:
         return jjMoveStringLiteralDfa3_0(active0, 0x400000L);
      case 116:
         return jjMoveStringLiteralDfa3_0(active0, 0x80000L);
      case 118:
         if ((active0 & 0x40000000L) != 0L)
            return jjStartNfaWithStates_0(2, 30, 1);
         break;
      default :
         break;
   }
   return jjStartNfa_0(1, active0);
}
private int jjMoveStringLiteralDfa3_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(1, old0);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(2, active0);
      return 3;
   }
   switch(curChar)
   {
      case 99:
         return jjMoveStringLiteralDfa4_0(active0, 0x400000L);
      case 101:
         if ((active0 & 0x4000000L) != 0L)
            return jjStartNfaWithStates_0(3, 26, 1);
         return jjMoveStringLiteralDfa4_0(active0, 0x900000L);
      case 102:
         return jjMoveStringLiteralDfa4_0(active0, 0x20000L);
      case 105:
         return jjMoveStringLiteralDfa4_0(active0, 0x200000L);
      case 107:
         return jjMoveStringLiteralDfa4_0(active0, 0x2000000L);
      case 108:
         return jjMoveStringLiteralDfa4_0(active0, 0x40000L);
      case 114:
         return jjMoveStringLiteralDfa4_0(active0, 0x80000L);
      case 115:
         return jjMoveStringLiteralDfa4_0(active0, 0x1000000L);
      default :
         break;
   }
   return jjStartNfa_0(2, active0);
}
private int jjMoveStringLiteralDfa4_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(2, old0);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(3, active0);
      return 4;
   }
   switch(curChar)
   {
      case 58:
         return jjMoveStringLiteralDfa5_0(active0, 0x20000L);
      case 97:
         return jjMoveStringLiteralDfa5_0(active0, 0x2200000L);
      case 100:
         return jjMoveStringLiteralDfa5_0(active0, 0x40000L);
      case 101:
         return jjMoveStringLiteralDfa5_0(active0, 0x400000L);
      case 105:
         return jjMoveStringLiteralDfa5_0(active0, 0x80000L);
      case 110:
         return jjMoveStringLiteralDfa5_0(active0, 0x800000L);
      case 114:
         return jjMoveStringLiteralDfa5_0(active0, 0x100000L);
      case 115:
         return jjMoveStringLiteralDfa5_0(active0, 0x1000000L);
      default :
         break;
   }
   return jjStartNfa_0(3, active0);
}
private int jjMoveStringLiteralDfa5_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(3, old0);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(4, active0);
      return 5;
   }
   switch(curChar)
   {
      case 58:
         if ((active0 & 0x20000L) != 0L)
            return jjStopAtPos(5, 17);
         return jjMoveStringLiteralDfa6_0(active0, 0x1040000L);
      case 98:
         return jjMoveStringLiteralDfa6_0(active0, 0x280000L);
      case 101:
         return jjMoveStringLiteralDfa6_0(active0, 0x100000L);
      case 103:
         return jjMoveStringLiteralDfa6_0(active0, 0x2000000L);
      case 110:
         return jjMoveStringLiteralDfa6_0(active0, 0x400000L);
      case 116:
         return jjMoveStringLiteralDfa6_0(active0, 0x800000L);
      default :
         break;
   }
   return jjStartNfa_0(4, active0);
}
private int jjMoveStringLiteralDfa6_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(4, old0);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(5, active0);
      return 6;
   }
   switch(curChar)
   {
      case 58:
         if ((active0 & 0x40000L) != 0L)
            return jjStopAtPos(6, 18);
         else if ((active0 & 0x1000000L) != 0L)
            return jjStopAtPos(6, 24);
         return jjMoveStringLiteralDfa7_0(active0, 0x800000L);
      case 100:
         return jjMoveStringLiteralDfa7_0(active0, 0x400000L);
      case 101:
         return jjMoveStringLiteralDfa7_0(active0, 0x2000000L);
      case 108:
         return jjMoveStringLiteralDfa7_0(active0, 0x200000L);
      case 110:
         return jjMoveStringLiteralDfa7_0(active0, 0x100000L);
      case 117:
         return jjMoveStringLiteralDfa7_0(active0, 0x80000L);
      default :
         break;
   }
   return jjStartNfa_0(5, active0);
}
private int jjMoveStringLiteralDfa7_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(5, old0);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(6, active0);
      return 7;
   }
   switch(curChar)
   {
      case 58:
         if ((active0 & 0x800000L) != 0L)
            return jjStopAtPos(7, 23);
         return jjMoveStringLiteralDfa8_0(active0, 0x2000000L);
      case 97:
         return jjMoveStringLiteralDfa8_0(active0, 0x400000L);
      case 99:
         return jjMoveStringLiteralDfa8_0(active0, 0x100000L);
      case 101:
         return jjMoveStringLiteralDfa8_0(active0, 0x200000L);
      case 116:
         return jjMoveStringLiteralDfa8_0(active0, 0x80000L);
      default :
         break;
   }
   return jjStartNfa_0(6, active0);
}
private int jjMoveStringLiteralDfa8_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(6, old0);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(7, active0);
      return 8;
   }
   switch(curChar)
   {
      case 58:
         if ((active0 & 0x2000000L) != 0L)
            return jjStopAtPos(8, 25);
         return jjMoveStringLiteralDfa9_0(active0, 0x200000L);
      case 101:
         return jjMoveStringLiteralDfa9_0(active0, 0x180000L);
      case 110:
         return jjMoveStringLiteralDfa9_0(active0, 0x400000L);
      default :
         break;
   }
   return jjStartNfa_0(7, active0);
}
private int jjMoveStringLiteralDfa9_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(7, old0);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(8, active0);
      return 9;
   }
   switch(curChar)
   {
      case 58:
         if ((active0 & 0x200000L) != 0L)
            return jjStopAtPos(9, 21);
         return jjMoveStringLiteralDfa10_0(active0, 0x180000L);
      case 116:
         return jjMoveStringLiteralDfa10_0(active0, 0x400000L);
      default :
         break;
   }
   return jjStartNfa_0(8, active0);
}
private int jjMoveStringLiteralDfa10_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(8, old0);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(9, active0);
      return 10;
   }
   switch(curChar)
   {
      case 45:
         return jjMoveStringLiteralDfa11_0(active0, 0x400000L);
      case 58:
         if ((active0 & 0x80000L) != 0L)
            return jjStopAtPos(10, 19);
         else if ((active0 & 0x100000L) != 0L)
            return jjStopAtPos(10, 20);
         break;
      default :
         break;
   }
   return jjStartNfa_0(9, active0);
}
private int jjMoveStringLiteralDfa11_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(9, old0);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(10, active0);
      return 11;
   }
   switch(curChar)
   {
      case 111:
         return jjMoveStringLiteralDfa12_0(active0, 0x400000L);
      default :
         break;
   }
   return jjStartNfa_0(10, active0);
}
private int jjMoveStringLiteralDfa12_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(10, old0);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(11, active0);
      return 12;
   }
   switch(curChar)
   {
      case 114:
         return jjMoveStringLiteralDfa13_0(active0, 0x400000L);
      default :
         break;
   }
   return jjStartNfa_0(11, active0);
}
private int jjMoveStringLiteralDfa13_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(11, old0);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(12, active0);
      return 13;
   }
   switch(curChar)
   {
      case 45:
         return jjMoveStringLiteralDfa14_0(active0, 0x400000L);
      default :
         break;
   }
   return jjStartNfa_0(12, active0);
}
private int jjMoveStringLiteralDfa14_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(12, old0);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(13, active0);
      return 14;
   }
   switch(curChar)
   {
      case 115:
         return jjMoveStringLiteralDfa15_0(active0, 0x400000L);
      default :
         break;
   }
   return jjStartNfa_0(13, active0);
}
private int jjMoveStringLiteralDfa15_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(13, old0);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(14, active0);
      return 15;
   }
   switch(curChar)
   {
      case 101:
         return jjMoveStringLiteralDfa16_0(active0, 0x400000L);
      default :
         break;
   }
   return jjStartNfa_0(14, active0);
}
private int jjMoveStringLiteralDfa16_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(14, old0);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(15, active0);
      return 16;
   }
   switch(curChar)
   {
      case 108:
         return jjMoveStringLiteralDfa17_0(active0, 0x400000L);
      default :
         break;
   }
   return jjStartNfa_0(15, active0);
}
private int jjMoveStringLiteralDfa17_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(15, old0);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(16, active0);
      return 17;
   }
   switch(curChar)
   {
      case 102:
         return jjMoveStringLiteralDfa18_0(active0, 0x400000L);
      default :
         break;
   }
   return jjStartNfa_0(16, active0);
}
private int jjMoveStringLiteralDfa18_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(16, old0);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(17, active0);
      return 18;
   }
   switch(curChar)
   {
      case 58:
         return jjMoveStringLiteralDfa19_0(active0, 0x400000L);
      default :
         break;
   }
   return jjStartNfa_0(17, active0);
}
private int jjMoveStringLiteralDfa19_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(17, old0);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(18, active0);
      return 19;
   }
   switch(curChar)
   {
      case 58:
         if ((active0 & 0x400000L) != 0L)
            return jjStopAtPos(19, 22);
         break;
      default :
         break;
   }
   return jjStartNfa_0(18, active0);
}
private int jjStartNfaWithStates_0(int pos, int kind, int state)
{
   jjmatchedKind = kind;
   jjmatchedPos = pos;
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) { return pos + 1; }
   return jjMoveNfa_0(state, pos + 1);
}
static final long[] jjbitVec0 = {
   0xfffffffffffffffeL, 0xffffffffffffffffL, 0xffffffffffffffffL, 0xffffffffffffffffL
};
static final long[] jjbitVec2 = {
   0x0L, 0x0L, 0xffffffffffffffffL, 0xffffffffffffffffL
};
private int jjMoveNfa_0(int startState, int curPos)
{
   int startsAt = 0;
   jjnewStateCnt = 11;
   int i = 1;
   jjstateSet[0] = startState;
   int kind = 0x7fffffff;
   for (;;)
   {
      if (++jjround == 0x7fffffff)
         ReInitRounds();
      if (curChar < 64)
      {
         long l = 1L << curChar;
         do
         {
            switch(jjstateSet[--i])
            {
               case 0:
                  if ((0x3ff000000000000L & l) != 0L)
                  {
                     if (kind > 40)
                        kind = 40;
                     jjCheckNAdd(9);
                  }
                  else if (curChar == 39)
                     jjCheckNAddTwoStates(6, 7);
                  else if (curChar == 34)
                     jjCheckNAddTwoStates(3, 4);
                  break;
               case 1:
                  if ((0x3ff001000000000L & l) == 0L)
                     break;
                  if (kind > 31)
                     kind = 31;
                  jjstateSet[jjnewStateCnt++] = 1;
                  break;
               case 2:
                  if (curChar == 34)
                     jjCheckNAddTwoStates(3, 4);
                  break;
               case 3:
                  if ((0xfffffffbffffffffL & l) != 0L)
                     jjCheckNAddTwoStates(3, 4);
                  break;
               case 4:
                  if (curChar == 34 && kind > 38)
                     kind = 38;
                  break;
               case 5:
                  if (curChar == 39)
                     jjCheckNAddTwoStates(6, 7);
                  break;
               case 6:
                  if ((0xffffff7fffffffffL & l) != 0L)
                     jjCheckNAddTwoStates(6, 7);
                  break;
               case 7:
                  if (curChar == 39 && kind > 38)
                     kind = 38;
                  break;
               case 8:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 40)
                     kind = 40;
                  jjCheckNAdd(9);
                  break;
               case 9:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 40)
                     kind = 40;
                  jjCheckNAddTwoStates(10, 9);
                  break;
               case 10:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 40)
                     kind = 40;
                  jjCheckNAdd(10);
                  break;
               default : break;
            }
         } while(i != startsAt);
      }
      else if (curChar < 128)
      {
         long l = 1L << (curChar & 077);
         do
         {
            switch(jjstateSet[--i])
            {
               case 0:
               case 1:
                  if ((0x7fffffe87fffffeL & l) == 0L)
                     break;
                  if (kind > 31)
                     kind = 31;
                  jjCheckNAdd(1);
                  break;
               case 3:
                  jjAddStates(0, 1);
                  break;
               case 6:
                  jjAddStates(2, 3);
                  break;
               default : break;
            }
         } while(i != startsAt);
      }
      else
      {
         int hiByte = (int)(curChar >> 8);
         int i1 = hiByte >> 6;
         long l1 = 1L << (hiByte & 077);
         int i2 = (curChar & 0xff) >> 6;
         long l2 = 1L << (curChar & 077);
         do
         {
            switch(jjstateSet[--i])
            {
               case 3:
                  if (jjCanMove_0(hiByte, i1, i2, l1, l2))
                     jjAddStates(0, 1);
                  break;
               case 6:
                  if (jjCanMove_0(hiByte, i1, i2, l1, l2))
                     jjAddStates(2, 3);
                  break;
               default : break;
            }
         } while(i != startsAt);
      }
      if (kind != 0x7fffffff)
      {
         jjmatchedKind = kind;
         jjmatchedPos = curPos;
         kind = 0x7fffffff;
      }
      ++curPos;
      if ((i = jjnewStateCnt) == (startsAt = 11 - (jjnewStateCnt = startsAt)))
         return curPos;
      try { curChar = input_stream.readChar(); }
      catch(java.io.IOException e) { return curPos; }
   }
}
static final int[] jjnextStates = {
   3, 4, 6, 7, 
};
private static final boolean jjCanMove_0(int hiByte, int i1, int i2, long l1, long l2)
{
   switch(hiByte)
   {
      case 0:
         return ((jjbitVec2[i2] & l2) != 0L);
      default :
         if ((jjbitVec0[i1] & l1) != 0L)
            return true;
         return false;
   }
}

/** Token literal values. */
public static final String[] jjstrLiteralImages = {
"", "\52", "\50", "\51", "\56", "\56\56", "\100", "\46", "\44", "\133", 
"\135", "\54", null, null, null, null, null, "\163\145\154\146\72\72", 
"\143\150\151\154\144\72\72", "\141\164\164\162\151\142\165\164\145\72\72", 
"\162\145\146\145\162\145\156\143\145\72\72", "\166\141\162\151\141\142\154\145\72\72", 
"\144\145\163\143\145\156\144\141\156\164\55\157\162\55\163\145\154\146\72\72", "\160\141\162\145\156\164\72\72", "\143\154\141\163\163\72\72", 
"\160\141\143\153\141\147\145\72\72", "\156\157\144\145", "\157\162", "\141\156\144", "\155\157\144", 
"\144\151\166", null, null, null, "\57", "\57\57", "\75", "\41\75", null, null, null, };

/** Lexer state names. */
public static final String[] lexStateNames = {
   "DEFAULT",
};
static final long[] jjtoToken = {
   0x17cfffe0fffL, 
};
static final long[] jjtoSkip = {
   0x1f000L, 
};
protected JavaCharStream input_stream;
private final int[] jjrounds = new int[11];
private final int[] jjstateSet = new int[22];
protected char curChar;
/** Constructor. */
public LocationParserTokenManager(JavaCharStream stream){
   if (JavaCharStream.staticFlag)
      throw new Error("ERROR: Cannot use a static CharStream class with a non-static lexical analyzer.");
   input_stream = stream;
}

/** Constructor. */
public LocationParserTokenManager(JavaCharStream stream, int lexState){
   this(stream);
   SwitchTo(lexState);
}

/** Reinitialise parser. */
public void ReInit(JavaCharStream stream)
{
   jjmatchedPos = jjnewStateCnt = 0;
   curLexState = defaultLexState;
   input_stream = stream;
   ReInitRounds();
}
private void ReInitRounds()
{
   int i;
   jjround = 0x80000001;
   for (i = 11; i-- > 0;)
      jjrounds[i] = 0x80000000;
}

/** Reinitialise parser. */
public void ReInit(JavaCharStream stream, int lexState)
{
   ReInit(stream);
   SwitchTo(lexState);
}

/** Switch to specified lex state. */
public void SwitchTo(int lexState)
{
   if (lexState >= 1 || lexState < 0)
      throw new TokenMgrError("Error: Ignoring invalid lexical state : " + lexState + ". State unchanged.", TokenMgrError.INVALID_LEXICAL_STATE);
   else
      curLexState = lexState;
}

protected Token jjFillToken()
{
   final Token t;
   final String curTokenImage;
   final int beginLine;
   final int endLine;
   final int beginColumn;
   final int endColumn;
   String im = jjstrLiteralImages[jjmatchedKind];
   curTokenImage = (im == null) ? input_stream.GetImage() : im;
   beginLine = input_stream.getBeginLine();
   beginColumn = input_stream.getBeginColumn();
   endLine = input_stream.getEndLine();
   endColumn = input_stream.getEndColumn();
   t = Token.newToken(jjmatchedKind, curTokenImage);

   t.beginLine = beginLine;
   t.endLine = endLine;
   t.beginColumn = beginColumn;
   t.endColumn = endColumn;

   return t;
}

int curLexState = 0;
int defaultLexState = 0;
int jjnewStateCnt;
int jjround;
int jjmatchedPos;
int jjmatchedKind;

/** Get the next Token. */
public Token getNextToken() 
{
  Token matchedToken;
  int curPos = 0;

  EOFLoop :
  for (;;)
  {
   try
   {
      curChar = input_stream.BeginToken();
   }
   catch(java.io.IOException e)
   {
      jjmatchedKind = 0;
      matchedToken = jjFillToken();
      return matchedToken;
   }

   try { input_stream.backup(0);
      while (curChar <= 32 && (0x100003600L & (1L << curChar)) != 0L)
         curChar = input_stream.BeginToken();
   }
   catch (java.io.IOException e1) { continue EOFLoop; }
   jjmatchedKind = 0x7fffffff;
   jjmatchedPos = 0;
   curPos = jjMoveStringLiteralDfa0_0();
   if (jjmatchedKind != 0x7fffffff)
   {
      if (jjmatchedPos + 1 < curPos)
         input_stream.backup(curPos - jjmatchedPos - 1);
      if ((jjtoToken[jjmatchedKind >> 6] & (1L << (jjmatchedKind & 077))) != 0L)
      {
         matchedToken = jjFillToken();
         return matchedToken;
      }
      else
      {
         continue EOFLoop;
      }
   }
   int error_line = input_stream.getEndLine();
   int error_column = input_stream.getEndColumn();
   String error_after = null;
   boolean EOFSeen = false;
   try { input_stream.readChar(); input_stream.backup(1); }
   catch (java.io.IOException e1) {
      EOFSeen = true;
      error_after = curPos <= 1 ? "" : input_stream.GetImage();
      if (curChar == '\n' || curChar == '\r') {
         error_line++;
         error_column = 0;
      }
      else
         error_column++;
   }
   if (!EOFSeen) {
      input_stream.backup(1);
      error_after = curPos <= 1 ? "" : input_stream.GetImage();
   }
   throw new TokenMgrError(EOFSeen, curLexState, error_line, error_column, error_after, curChar, TokenMgrError.LEXICAL_ERROR);
  }
}

private void jjCheckNAdd(int state)
{
   if (jjrounds[state] != jjround)
   {
      jjstateSet[jjnewStateCnt++] = state;
      jjrounds[state] = jjround;
   }
}
private void jjAddStates(int start, int end)
{
   do {
      jjstateSet[jjnewStateCnt++] = jjnextStates[start];
   } while (start++ != end);
}
private void jjCheckNAddTwoStates(int state1, int state2)
{
   jjCheckNAdd(state1);
   jjCheckNAdd(state2);
}

}
