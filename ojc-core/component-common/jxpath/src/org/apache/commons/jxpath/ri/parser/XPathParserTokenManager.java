/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)XPathParserTokenManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.parser;
import org.apache.commons.jxpath.ri.Compiler;
import java.util.ArrayList;

public class XPathParserTokenManager implements XPathParserConstants
{
  public  java.io.PrintStream debugStream = System.out;
  public  void setDebugStream(java.io.PrintStream ds) { debugStream = ds; }
private final int jjStopStringLiteralDfa_0(int pos, long active0, long active1)
{
   switch (pos)
   {
      case 0:
         if ((active1 & 0xc0000L) != 0L)
            return 10;
         if ((active0 & 0xfffffffff8000000L) != 0L || (active1 & 0x3fffL) != 0L)
         {
            jjmatchedKind = 78;
            return 12;
         }
         return -1;
      case 1:
         if ((active0 & 0x8000008000000L) != 0L)
            return 12;
         if ((active0 & 0xfff7fffff0000000L) != 0L || (active1 & 0x3fffL) != 0L)
         {
            jjmatchedKind = 78;
            jjmatchedPos = 1;
            return 12;
         }
         return -1;
      case 2:
         if ((active0 & 0x10000070000000L) != 0L || (active1 & 0x208L) != 0L)
            return 12;
         if ((active0 & 0xffe7ffff80000000L) != 0L || (active1 & 0x3df7L) != 0L)
         {
            jjmatchedKind = 78;
            jjmatchedPos = 2;
            return 12;
         }
         return -1;
      case 3:
         if ((active0 & 0xc1010180000000L) != 0L || (active1 & 0xd0L) != 0L)
            return 12;
         if ((active0 & 0xff26fefe00000000L) != 0L || (active1 & 0x3d27L) != 0L)
         {
            if (jjmatchedPos != 3)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 3;
            }
            return 12;
         }
         return -1;
      case 4:
         if ((active0 & 0xff62fff600000000L) != 0L || (active1 & 0x2907L) != 0L)
         {
            jjmatchedKind = 78;
            jjmatchedPos = 4;
            return 12;
         }
         if ((active0 & 0x4000000000000L) != 0L || (active1 & 0x1420L) != 0L)
            return 12;
         if ((active0 & 0x800000000L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 3;
            }
            return -1;
         }
         return -1;
      case 5:
         if ((active0 & 0x8300000000000000L) != 0L || (active1 & 0x100L) != 0L)
            return 12;
         if ((active0 & 0x7c62ffe600000000L) != 0L || (active1 & 0x2807L) != 0L)
         {
            if (jjmatchedPos != 5)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 5;
            }
            return 12;
         }
         if ((active0 & 0x1000000000L) != 0L)
         {
            if (jjmatchedPos < 4)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 4;
            }
            return -1;
         }
         if ((active0 & 0x800000000L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 3;
            }
            return -1;
         }
         return -1;
      case 6:
         if ((active0 & 0x200000000L) != 0L || (active1 & 0x804L) != 0L)
            return 12;
         if ((active0 & 0x2000000000L) != 0L)
         {
            if (jjmatchedPos < 5)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 5;
            }
            return -1;
         }
         if ((active0 & 0x1000000000L) != 0L)
         {
            if (jjmatchedPos < 4)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 4;
            }
            return -1;
         }
         if ((active0 & 0xfc62ffc400000000L) != 0L || (active1 & 0x2003L) != 0L)
         {
            jjmatchedKind = 78;
            jjmatchedPos = 6;
            return 12;
         }
         return -1;
      case 7:
         if ((active0 & 0xf460ffc400000000L) != 0L || (active1 & 0x2003L) != 0L)
         {
            jjmatchedKind = 78;
            jjmatchedPos = 7;
            return 12;
         }
         if ((active0 & 0x802000000000000L) != 0L)
            return 12;
         if ((active0 & 0x2000000000L) != 0L)
         {
            if (jjmatchedPos < 5)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 5;
            }
            return -1;
         }
         return -1;
      case 8:
         if ((active0 & 0x7000000000000000L) != 0L || (active1 & 0x2L) != 0L)
            return 12;
         if ((active0 & 0x4000000000L) != 0L)
         {
            if (jjmatchedPos < 7)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 7;
            }
            return -1;
         }
         if ((active0 & 0x8460ff8400000000L) != 0L || (active1 & 0x2001L) != 0L)
         {
            if (jjmatchedPos != 8)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 8;
            }
            return 12;
         }
         return -1;
      case 9:
         if ((active0 & 0x20000000000000L) != 0L)
            return 12;
         if ((active0 & 0x78000000000L) != 0L)
         {
            if (jjmatchedPos < 8)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 8;
            }
            return -1;
         }
         if ((active0 & 0x4000000000L) != 0L)
         {
            if (jjmatchedPos < 7)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 7;
            }
            return -1;
         }
         if ((active0 & 0xb440f80400000000L) != 0L || (active1 & 0x2001L) != 0L)
         {
            jjmatchedKind = 78;
            jjmatchedPos = 9;
            return 12;
         }
         return -1;
      case 10:
         if ((active0 & 0x400000000000000L) != 0L)
            return 12;
         if ((active0 & 0x80000000000L) != 0L)
         {
            if (jjmatchedPos < 9)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 9;
            }
            return -1;
         }
         if ((active0 & 0x78000000000L) != 0L)
         {
            if (jjmatchedPos < 8)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 8;
            }
            return -1;
         }
         if ((active0 & 0xb040f00400000000L) != 0L || (active1 & 0x2001L) != 0L)
         {
            jjmatchedKind = 78;
            jjmatchedPos = 10;
            return 12;
         }
         return -1;
      case 11:
         if ((active0 & 0xb040f00400000000L) != 0L || (active1 & 0x2001L) != 0L)
         {
            jjmatchedKind = 78;
            jjmatchedPos = 11;
            return 12;
         }
         if ((active0 & 0x80000000000L) != 0L)
         {
            if (jjmatchedPos < 9)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 9;
            }
            return -1;
         }
         return -1;
      case 12:
         if ((active0 & 0x8040000000000000L) != 0L || (active1 & 0x2000L) != 0L)
            return 12;
         if ((active0 & 0x3000f00400000000L) != 0L || (active1 & 0x1L) != 0L)
         {
            jjmatchedKind = 78;
            jjmatchedPos = 12;
            return 12;
         }
         return -1;
      case 13:
         if ((active0 & 0x3000f00400000000L) != 0L || (active1 & 0x1L) != 0L)
         {
            jjmatchedKind = 78;
            jjmatchedPos = 13;
            return 12;
         }
         return -1;
      case 14:
         if ((active0 & 0x2000000000000000L) != 0L || (active1 & 0x1L) != 0L)
            return 12;
         if ((active0 & 0x1000f00400000000L) != 0L)
         {
            jjmatchedKind = 78;
            jjmatchedPos = 14;
            return 12;
         }
         return -1;
      case 15:
         if ((active0 & 0x1000000000000000L) != 0L)
            return 12;
         if ((active0 & 0xf00400000000L) != 0L)
         {
            jjmatchedKind = 78;
            jjmatchedPos = 15;
            return 12;
         }
         return -1;
      case 16:
         if ((active0 & 0xe00400000000L) != 0L)
         {
            jjmatchedKind = 78;
            jjmatchedPos = 16;
            return 12;
         }
         if ((active0 & 0x100000000000L) != 0L)
         {
            if (jjmatchedPos < 15)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 15;
            }
            return -1;
         }
         return -1;
      case 17:
         if ((active0 & 0x600000000000L) != 0L)
         {
            if (jjmatchedPos < 16)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 16;
            }
            return -1;
         }
         if ((active0 & 0x100000000000L) != 0L)
         {
            if (jjmatchedPos < 15)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 15;
            }
            return -1;
         }
         if ((active0 & 0x800400000000L) != 0L)
         {
            jjmatchedKind = 78;
            jjmatchedPos = 17;
            return 12;
         }
         return -1;
      case 18:
         if ((active0 & 0x400000000L) != 0L)
         {
            jjmatchedKind = 78;
            jjmatchedPos = 18;
            return 12;
         }
         if ((active0 & 0x800000000000L) != 0L)
         {
            if (jjmatchedPos < 17)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 17;
            }
            return -1;
         }
         if ((active0 & 0x600000000000L) != 0L)
         {
            if (jjmatchedPos < 16)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 16;
            }
            return -1;
         }
         return -1;
      case 19:
         if ((active0 & 0x400000000L) != 0L)
         {
            jjmatchedKind = 78;
            jjmatchedPos = 19;
            return 12;
         }
         if ((active0 & 0x800000000000L) != 0L)
         {
            if (jjmatchedPos < 17)
            {
               jjmatchedKind = 78;
               jjmatchedPos = 17;
            }
            return -1;
         }
         return -1;
      case 20:
         if ((active0 & 0x400000000L) != 0L)
         {
            jjmatchedKind = 78;
            jjmatchedPos = 20;
            return 12;
         }
         return -1;
      default :
         return -1;
   }
}
private final int jjStartNfa_0(int pos, long active0, long active1)
{
   return jjMoveNfa_0(jjStopStringLiteralDfa_0(pos, active0, active1), pos + 1);
}
private final int jjStopAtPos(int pos, int kind)
{
   jjmatchedKind = kind;
   jjmatchedPos = pos;
   return pos + 1;
}
private final int jjStartNfaWithStates_0(int pos, int kind, int state)
{
   jjmatchedKind = kind;
   jjmatchedPos = pos;
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) { return pos + 1; }
   return jjMoveNfa_0(state, pos + 1);
}
private final int jjMoveStringLiteralDfa0_0()
{
   switch(curChar)
   {
      case 33:
         return jjMoveStringLiteralDfa1_0(0x1000L, 0x0L);
      case 36:
         return jjStopAtPos(0, 17);
      case 40:
         return jjStopAtPos(0, 80);
      case 41:
         return jjStopAtPos(0, 81);
      case 42:
         return jjStopAtPos(0, 88);
      case 43:
         return jjStopAtPos(0, 9);
      case 44:
         return jjStopAtPos(0, 87);
      case 45:
         return jjStopAtPos(0, 10);
      case 46:
         jjmatchedKind = 82;
         return jjMoveStringLiteralDfa1_0(0x0L, 0x80000L);
      case 47:
         jjmatchedKind = 6;
         return jjMoveStringLiteralDfa1_0(0x80L, 0x0L);
      case 58:
         return jjStopAtPos(0, 79);
      case 60:
         jjmatchedKind = 13;
         return jjMoveStringLiteralDfa1_0(0x4000L, 0x0L);
      case 61:
         return jjStopAtPos(0, 11);
      case 62:
         jjmatchedKind = 15;
         return jjMoveStringLiteralDfa1_0(0x10000L, 0x0L);
      case 64:
         return jjStopAtPos(0, 86);
      case 91:
         return jjStopAtPos(0, 84);
      case 93:
         return jjStopAtPos(0, 85);
      case 97:
         return jjMoveStringLiteralDfa1_0(0x10c010000000L, 0x0L);
      case 98:
         return jjMoveStringLiteralDfa1_0(0x0L, 0x4L);
      case 99:
         return jjMoveStringLiteralDfa1_0(0xa04001200000000L, 0x800L);
      case 100:
         return jjMoveStringLiteralDfa1_0(0x880040000000L, 0x0L);
      case 102:
         return jjMoveStringLiteralDfa1_0(0x240000000000L, 0x2420L);
      case 105:
         return jjMoveStringLiteralDfa1_0(0x8000000000000L, 0x0L);
      case 107:
         return jjMoveStringLiteralDfa1_0(0x10000000000000L, 0x0L);
      case 108:
         return jjMoveStringLiteralDfa1_0(0x21000000000000L, 0x80L);
      case 109:
         return jjMoveStringLiteralDfa1_0(0x20000000L, 0x0L);
      case 110:
         return jjMoveStringLiteralDfa1_0(0xc0010080000000L, 0x149L);
      case 111:
         return jjMoveStringLiteralDfa1_0(0x8000000L, 0x0L);
      case 112:
         return jjMoveStringLiteralDfa1_0(0x2422400000000L, 0x0L);
      case 114:
         return jjMoveStringLiteralDfa1_0(0x0L, 0x1000L);
      case 115:
         return jjMoveStringLiteralDfa1_0(0xf500000800000000L, 0x200L);
      case 116:
         return jjMoveStringLiteralDfa1_0(0x100000000L, 0x12L);
      case 124:
         return jjStopAtPos(0, 8);
      default :
         return jjMoveNfa_0(0, 0);
   }
}
private final int jjMoveStringLiteralDfa1_0(long active0, long active1)
{
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(0, active0, active1);
      return 1;
   }
   switch(curChar)
   {
      case 46:
         if ((active1 & 0x80000L) != 0L)
            return jjStopAtPos(1, 83);
         break;
      case 47:
         if ((active0 & 0x80L) != 0L)
            return jjStopAtPos(1, 7);
         break;
      case 61:
         if ((active0 & 0x1000L) != 0L)
            return jjStopAtPos(1, 12);
         else if ((active0 & 0x4000L) != 0L)
            return jjStopAtPos(1, 14);
         else if ((active0 & 0x10000L) != 0L)
            return jjStopAtPos(1, 16);
         break;
      case 97:
         return jjMoveStringLiteralDfa2_0(active0, 0xc1012000000000L, active1, 0xa0L);
      case 100:
         if ((active0 & 0x8000000000000L) != 0L)
            return jjStartNfaWithStates_0(1, 51, 12);
         break;
      case 101:
         return jjMoveStringLiteralDfa2_0(active0, 0x10880900000000L, active1, 0x800L);
      case 104:
         return jjMoveStringLiteralDfa2_0(active0, 0x1000000000L, active1, 0L);
      case 105:
         return jjMoveStringLiteralDfa2_0(active0, 0x40000000L, active1, 0L);
      case 108:
         return jjMoveStringLiteralDfa2_0(active0, 0L, active1, 0x400L);
      case 110:
         return jjMoveStringLiteralDfa2_0(active0, 0x104010000000L, active1, 0L);
      case 111:
         return jjMoveStringLiteralDfa2_0(active0, 0xa262402a0000000L, active1, 0x300dL);
      case 114:
         if ((active0 & 0x8000000L) != 0L)
            return jjStartNfaWithStates_0(1, 27, 12);
         return jjMoveStringLiteralDfa2_0(active0, 0x420400000000L, active1, 0x12L);
      case 116:
         return jjMoveStringLiteralDfa2_0(active0, 0x8500008000000000L, active1, 0L);
      case 117:
         return jjMoveStringLiteralDfa2_0(active0, 0x7000000000000000L, active1, 0x340L);
      default :
         break;
   }
   return jjStartNfa_0(0, active0, active1);
}
private final int jjMoveStringLiteralDfa2_0(long old0, long active0, long old1, long active1)
{
   if (((active0 &= old0) | (active1 &= old1)) == 0L)
      return jjStartNfa_0(0, old0, old1); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(1, active0, active1);
      return 2;
   }
   switch(curChar)
   {
      case 97:
         return jjMoveStringLiteralDfa3_0(active0, 0x400000000000000L, active1, 0x2L);
      case 98:
         return jjMoveStringLiteralDfa3_0(active0, 0x7000000000000000L, active1, 0L);
      case 99:
         return jjMoveStringLiteralDfa3_0(active0, 0x20104000000000L, active1, 0L);
      case 100:
         if ((active0 & 0x10000000L) != 0L)
            return jjStartNfaWithStates_0(2, 28, 12);
         else if ((active0 & 0x20000000L) != 0L)
            return jjStartNfaWithStates_0(2, 29, 12);
         return jjMoveStringLiteralDfa3_0(active0, 0x80000000L, active1, 0L);
      case 101:
         return jjMoveStringLiteralDfa3_0(active0, 0x420000000000L, active1, 0L);
      case 105:
         return jjMoveStringLiteralDfa3_0(active0, 0x1000000000L, active1, 0x800L);
      case 108:
         return jjMoveStringLiteralDfa3_0(active0, 0x240800000000L, active1, 0x60L);
      case 109:
         if ((active1 & 0x200L) != 0L)
            return jjStartNfaWithStates_0(2, 73, 12);
         return jjMoveStringLiteralDfa3_0(active0, 0xc0010200000000L, active1, 0x100L);
      case 110:
         return jjMoveStringLiteralDfa3_0(active0, 0xa00000000000000L, active1, 0x80L);
      case 111:
         return jjMoveStringLiteralDfa3_0(active0, 0x400000000L, active1, 0x404L);
      case 114:
         return jjMoveStringLiteralDfa3_0(active0, 0x8100002000000000L, active1, 0x2001L);
      case 115:
         return jjMoveStringLiteralDfa3_0(active0, 0x3880000000000L, active1, 0L);
      case 116:
         if ((active1 & 0x8L) != 0L)
            return jjStartNfaWithStates_0(2, 67, 12);
         return jjMoveStringLiteralDfa3_0(active0, 0x8000000000L, active1, 0L);
      case 117:
         return jjMoveStringLiteralDfa3_0(active0, 0x4000000000000L, active1, 0x1010L);
      case 118:
         if ((active0 & 0x40000000L) != 0L)
            return jjStartNfaWithStates_0(2, 30, 12);
         break;
      case 120:
         return jjMoveStringLiteralDfa3_0(active0, 0x100000000L, active1, 0L);
      case 121:
         if ((active0 & 0x10000000000000L) != 0L)
            return jjStartNfaWithStates_0(2, 52, 12);
         break;
      default :
         break;
   }
   return jjStartNfa_0(1, active0, active1);
}
private final int jjMoveStringLiteralDfa3_0(long old0, long active0, long old1, long active1)
{
   if (((active0 &= old0) | (active1 &= old1)) == 0L)
      return jjStartNfa_0(1, old0, old1); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(2, active0, active1);
      return 3;
   }
   switch(curChar)
   {
      case 97:
         return jjMoveStringLiteralDfa4_0(active0, 0x20000000000000L, active1, 0L);
      case 98:
         return jjMoveStringLiteralDfa4_0(active0, 0L, active1, 0x100L);
      case 99:
         return jjMoveStringLiteralDfa4_0(active0, 0x200ca0400000000L, active1, 0L);
      case 101:
         if ((active0 & 0x80000000L) != 0L)
            return jjStartNfaWithStates_0(3, 31, 12);
         else if ((active0 & 0x80000000000000L) != 0L)
         {
            jjmatchedKind = 55;
            jjmatchedPos = 3;
         }
         else if ((active1 & 0x10L) != 0L)
            return jjStartNfaWithStates_0(3, 68, 12);
         return jjMoveStringLiteralDfa4_0(active0, 0x40116000000000L, active1, 0L);
      case 102:
         return jjMoveStringLiteralDfa4_0(active0, 0x800000000L, active1, 0L);
      case 103:
         if ((active1 & 0x80L) != 0L)
            return jjStartNfaWithStates_0(3, 71, 12);
         break;
      case 105:
         return jjMoveStringLiteralDfa4_0(active0, 0x8102000000000000L, active1, 0L);
      case 108:
         if ((active1 & 0x40L) != 0L)
            return jjStartNfaWithStates_0(3, 70, 12);
         return jjMoveStringLiteralDfa4_0(active0, 0x241000000000L, active1, 0x804L);
      case 109:
         return jjMoveStringLiteralDfa4_0(active0, 0x200000000L, active1, 0x2001L);
      case 110:
         return jjMoveStringLiteralDfa4_0(active0, 0x4000000000000L, active1, 0x1002L);
      case 111:
         return jjMoveStringLiteralDfa4_0(active0, 0L, active1, 0x400L);
      case 114:
         return jjMoveStringLiteralDfa4_0(active0, 0x400008000000000L, active1, 0L);
      case 115:
         return jjMoveStringLiteralDfa4_0(active0, 0x7000000000000000L, active1, 0x20L);
      case 116:
         if ((active0 & 0x100000000L) != 0L)
            return jjStartNfaWithStates_0(3, 32, 12);
         else if ((active0 & 0x1000000000000L) != 0L)
            return jjStartNfaWithStates_0(3, 48, 12);
         return jjMoveStringLiteralDfa4_0(active0, 0x800000000000000L, active1, 0L);
      default :
         break;
   }
   return jjStartNfa_0(2, active0, active1);
}
private final int jjMoveStringLiteralDfa4_0(long old0, long active0, long old1, long active1)
{
   if (((active0 &= old0) | (active1 &= old1)) == 0L)
      return jjStartNfa_0(2, old0, old1); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(3, active0, active1);
      return 4;
   }
   switch(curChar)
   {
      case 58:
         return jjMoveStringLiteralDfa5_0(active0, 0x800000000L, active1, 0L);
      case 97:
         return jjMoveStringLiteralDfa5_0(active0, 0xa00000000000000L, active1, 0x2001L);
      case 100:
         if ((active1 & 0x1000L) != 0L)
            return jjStartNfaWithStates_0(4, 76, 12);
         return jjMoveStringLiteralDfa5_0(active0, 0x1000000000L, active1, 0L);
      case 101:
         if ((active1 & 0x20L) != 0L)
            return jjStartNfaWithStates_0(4, 69, 12);
         return jjMoveStringLiteralDfa5_0(active0, 0xca0600000000L, active1, 0x104L);
      case 105:
         return jjMoveStringLiteralDfa5_0(active0, 0x8000000000L, active1, 0x800L);
      case 108:
         return jjMoveStringLiteralDfa5_0(active0, 0x20000000000000L, active1, 0L);
      case 110:
         return jjMoveStringLiteralDfa5_0(active0, 0x8100002000000000L, active1, 0L);
      case 111:
         return jjMoveStringLiteralDfa5_0(active0, 0x240000000000L, active1, 0L);
      case 114:
         if ((active1 & 0x400L) != 0L)
            return jjStartNfaWithStates_0(4, 74, 12);
         break;
      case 115:
         return jjMoveStringLiteralDfa5_0(active0, 0x40114000000000L, active1, 0x2L);
      case 116:
         if ((active0 & 0x4000000000000L) != 0L)
            return jjStartNfaWithStates_0(4, 50, 12);
         return jjMoveStringLiteralDfa5_0(active0, 0x7402000000000000L, active1, 0L);
      default :
         break;
   }
   return jjStartNfa_0(3, active0, active1);
}
private final int jjMoveStringLiteralDfa5_0(long old0, long active0, long old1, long active1)
{
   if (((active0 &= old0) | (active1 &= old1)) == 0L)
      return jjStartNfa_0(3, old0, old1); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(4, active0, active1);
      return 5;
   }
   switch(curChar)
   {
      case 45:
         return jjMoveStringLiteralDfa6_0(active0, 0x20000000000000L, active1, 0L);
      case 58:
         if ((active0 & 0x800000000L) != 0L)
            return jjStopAtPos(5, 35);
         return jjMoveStringLiteralDfa6_0(active0, 0x1000000000L, active1, 0L);
      case 97:
         return jjMoveStringLiteralDfa6_0(active0, 0L, active1, 0x4L);
      case 98:
         return jjMoveStringLiteralDfa6_0(active0, 0x8000000000L, active1, 0L);
      case 100:
         return jjMoveStringLiteralDfa6_0(active0, 0x420000000000L, active1, 0L);
      case 103:
         if ((active0 & 0x100000000000000L) != 0L)
         {
            jjmatchedKind = 56;
            jjmatchedPos = 5;
         }
         return jjMoveStringLiteralDfa6_0(active0, 0x8000000000000000L, active1, 0L);
      case 105:
         return jjMoveStringLiteralDfa6_0(active0, 0x802000000000000L, active1, 0L);
      case 108:
         return jjMoveStringLiteralDfa6_0(active0, 0L, active1, 0x3L);
      case 110:
         return jjMoveStringLiteralDfa6_0(active0, 0x880200000000L, active1, 0x800L);
      case 112:
         return jjMoveStringLiteralDfa6_0(active0, 0x40010000000000L, active1, 0L);
      case 114:
         if ((active1 & 0x100L) != 0L)
            return jjStartNfaWithStates_0(5, 72, 12);
         return jjMoveStringLiteralDfa6_0(active0, 0x7000000000000000L, active1, 0L);
      case 115:
         return jjMoveStringLiteralDfa6_0(active0, 0x400000400000000L, active1, 0L);
      case 116:
         if ((active0 & 0x200000000000000L) != 0L)
            return jjStartNfaWithStates_0(5, 57, 12);
         return jjMoveStringLiteralDfa6_0(active0, 0x106000000000L, active1, 0x2000L);
      case 119:
         return jjMoveStringLiteralDfa6_0(active0, 0x240000000000L, active1, 0L);
      default :
         break;
   }
   return jjStartNfa_0(4, active0, active1);
}
private final int jjMoveStringLiteralDfa6_0(long old0, long active0, long old1, long active1)
{
   if (((active0 &= old0) | (active1 &= old1)) == 0L)
      return jjStartNfa_0(4, old0, old1); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(5, active0, active1);
      return 6;
   }
   switch(curChar)
   {
      case 45:
         return jjMoveStringLiteralDfa7_0(active0, 0x8400000000000000L, active1, 0x2000L);
      case 58:
         if ((active0 & 0x1000000000L) != 0L)
            return jjStopAtPos(6, 36);
         return jjMoveStringLiteralDfa7_0(active0, 0x2000000000L, active1, 0L);
      case 97:
         return jjMoveStringLiteralDfa7_0(active0, 0x40010000000000L, active1, 0x2L);
      case 100:
         return jjMoveStringLiteralDfa7_0(active0, 0x880000000000L, active1, 0L);
      case 103:
         if ((active1 & 0x800L) != 0L)
            return jjStartNfaWithStates_0(6, 75, 12);
         break;
      case 105:
         return jjMoveStringLiteralDfa7_0(active0, 0x7000660000000000L, active1, 0x1L);
      case 110:
         if ((active1 & 0x4L) != 0L)
            return jjStartNfaWithStates_0(6, 66, 12);
         return jjMoveStringLiteralDfa7_0(active0, 0x820000000000000L, active1, 0L);
      case 111:
         return jjMoveStringLiteralDfa7_0(active0, 0x2104000000000L, active1, 0L);
      case 115:
         return jjMoveStringLiteralDfa7_0(active0, 0x400000000L, active1, 0L);
      case 116:
         if ((active0 & 0x200000000L) != 0L)
            return jjStartNfaWithStates_0(6, 33, 12);
         break;
      case 117:
         return jjMoveStringLiteralDfa7_0(active0, 0x8000000000L, active1, 0L);
      default :
         break;
   }
   return jjStartNfa_0(5, active0, active1);
}
private final int jjMoveStringLiteralDfa7_0(long old0, long active0, long old1, long active1)
{
   if (((active0 &= old0) | (active1 &= old1)) == 0L)
      return jjStartNfa_0(5, old0, old1); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(6, active0, active1);
      return 7;
   }
   switch(curChar)
   {
      case 58:
         if ((active0 & 0x2000000000L) != 0L)
            return jjStopAtPos(7, 37);
         break;
      case 97:
         return jjMoveStringLiteralDfa8_0(active0, 0x20880000000000L, active1, 0L);
      case 99:
         return jjMoveStringLiteralDfa8_0(active0, 0x40010000000000L, active1, 0L);
      case 105:
         return jjMoveStringLiteralDfa8_0(active0, 0x400000000L, active1, 0L);
      case 108:
         return jjMoveStringLiteralDfa8_0(active0, 0x8000000000000000L, active1, 0L);
      case 110:
         if ((active0 & 0x2000000000000L) != 0L)
            return jjStartNfaWithStates_0(7, 49, 12);
         return jjMoveStringLiteralDfa8_0(active0, 0x7000660000000000L, active1, 0x2000L);
      case 114:
         return jjMoveStringLiteralDfa8_0(active0, 0x104000000000L, active1, 0L);
      case 115:
         if ((active0 & 0x800000000000000L) != 0L)
            return jjStartNfaWithStates_0(7, 59, 12);
         break;
      case 116:
         return jjMoveStringLiteralDfa8_0(active0, 0x8000000000L, active1, 0x2L);
      case 119:
         return jjMoveStringLiteralDfa8_0(active0, 0x400000000000000L, active1, 0L);
      case 122:
         return jjMoveStringLiteralDfa8_0(active0, 0L, active1, 0x1L);
      default :
         break;
   }
   return jjStartNfa_0(6, active0, active1);
}
private final int jjMoveStringLiteralDfa8_0(long old0, long active0, long old1, long active1)
{
   if (((active0 &= old0) | (active1 &= old1)) == 0L)
      return jjStartNfa_0(6, old0, old1); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(7, active0, active1);
      return 8;
   }
   switch(curChar)
   {
      case 45:
         return jjMoveStringLiteralDfa9_0(active0, 0x100000000000L, active1, 0L);
      case 58:
         return jjMoveStringLiteralDfa9_0(active0, 0x4000000000L, active1, 0L);
      case 101:
         if ((active1 & 0x2L) != 0L)
            return jjStartNfaWithStates_0(8, 65, 12);
         return jjMoveStringLiteralDfa9_0(active0, 0x8040018000000000L, active1, 0x1L);
      case 103:
         if ((active0 & 0x4000000000000000L) != 0L)
         {
            jjmatchedKind = 62;
            jjmatchedPos = 8;
         }
         return jjMoveStringLiteralDfa9_0(active0, 0x3000660000000000L, active1, 0L);
      case 105:
         return jjMoveStringLiteralDfa9_0(active0, 0x400000000000000L, active1, 0L);
      case 109:
         return jjMoveStringLiteralDfa9_0(active0, 0x20000000000000L, active1, 0L);
      case 110:
         return jjMoveStringLiteralDfa9_0(active0, 0x880400000000L, active1, 0L);
      case 117:
         return jjMoveStringLiteralDfa9_0(active0, 0L, active1, 0x2000L);
      default :
         break;
   }
   return jjStartNfa_0(7, active0, active1);
}
private final int jjMoveStringLiteralDfa9_0(long old0, long active0, long old1, long active1)
{
   if (((active0 &= old0) | (active1 &= old1)) == 0L)
      return jjStartNfa_0(7, old0, old1); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(8, active0, active1);
      return 9;
   }
   switch(curChar)
   {
      case 45:
         return jjMoveStringLiteralDfa10_0(active0, 0x3040600000000000L, active1, 0x1L);
      case 58:
         if ((active0 & 0x4000000000L) != 0L)
            return jjStopAtPos(9, 38);
         return jjMoveStringLiteralDfa10_0(active0, 0x78000000000L, active1, 0L);
      case 101:
         if ((active0 & 0x20000000000000L) != 0L)
            return jjStartNfaWithStates_0(9, 53, 12);
         break;
      case 103:
         return jjMoveStringLiteralDfa10_0(active0, 0x400000000L, active1, 0L);
      case 109:
         return jjMoveStringLiteralDfa10_0(active0, 0L, active1, 0x2000L);
      case 110:
         return jjMoveStringLiteralDfa10_0(active0, 0x8000000000000000L, active1, 0L);
      case 111:
         return jjMoveStringLiteralDfa10_0(active0, 0x100000000000L, active1, 0L);
      case 116:
         return jjMoveStringLiteralDfa10_0(active0, 0x400880000000000L, active1, 0L);
      default :
         break;
   }
   return jjStartNfa_0(8, active0, active1);
}
private final int jjMoveStringLiteralDfa10_0(long old0, long active0, long old1, long active1)
{
   if (((active0 &= old0) | (active1 &= old1)) == 0L)
      return jjStartNfa_0(8, old0, old1); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(9, active0, active1);
      return 10;
   }
   switch(curChar)
   {
      case 45:
         return jjMoveStringLiteralDfa11_0(active0, 0x800400000000L, active1, 0L);
      case 58:
         if ((active0 & 0x8000000000L) != 0L)
            return jjStopAtPos(10, 39);
         else if ((active0 & 0x10000000000L) != 0L)
            return jjStopAtPos(10, 40);
         else if ((active0 & 0x20000000000L) != 0L)
            return jjStopAtPos(10, 41);
         else if ((active0 & 0x40000000000L) != 0L)
            return jjStopAtPos(10, 42);
         return jjMoveStringLiteralDfa11_0(active0, 0x80000000000L, active1, 0L);
      case 97:
         return jjMoveStringLiteralDfa11_0(active0, 0x2000000000000000L, active1, 0L);
      case 98:
         return jjMoveStringLiteralDfa11_0(active0, 0x1000000000000000L, active1, 0x2000L);
      case 103:
         return jjMoveStringLiteralDfa11_0(active0, 0x8000000000000000L, active1, 0L);
      case 104:
         if ((active0 & 0x400000000000000L) != 0L)
            return jjStartNfaWithStates_0(10, 58, 12);
         break;
      case 114:
         return jjMoveStringLiteralDfa11_0(active0, 0x100000000000L, active1, 0L);
      case 115:
         return jjMoveStringLiteralDfa11_0(active0, 0x600000000000L, active1, 0x1L);
      case 117:
         return jjMoveStringLiteralDfa11_0(active0, 0x40000000000000L, active1, 0L);
      default :
         break;
   }
   return jjStartNfa_0(9, active0, active1);
}
private final int jjMoveStringLiteralDfa11_0(long old0, long active0, long old1, long active1)
{
   if (((active0 &= old0) | (active1 &= old1)) == 0L)
      return jjStartNfa_0(9, old0, old1); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(10, active0, active1);
      return 11;
   }
   switch(curChar)
   {
      case 45:
         return jjMoveStringLiteralDfa12_0(active0, 0x100000000000L, active1, 0L);
      case 58:
         if ((active0 & 0x80000000000L) != 0L)
            return jjStopAtPos(11, 43);
         break;
      case 101:
         return jjMoveStringLiteralDfa12_0(active0, 0x1000000000000000L, active1, 0x2000L);
      case 102:
         return jjMoveStringLiteralDfa12_0(active0, 0x2000000000000000L, active1, 0L);
      case 105:
         return jjMoveStringLiteralDfa12_0(active0, 0x600400000000L, active1, 0L);
      case 111:
         return jjMoveStringLiteralDfa12_0(active0, 0x800000000000L, active1, 0L);
      case 112:
         return jjMoveStringLiteralDfa12_0(active0, 0L, active1, 0x1L);
      case 114:
         return jjMoveStringLiteralDfa12_0(active0, 0x40000000000000L, active1, 0L);
      case 116:
         return jjMoveStringLiteralDfa12_0(active0, 0x8000000000000000L, active1, 0L);
      default :
         break;
   }
   return jjStartNfa_0(10, active0, active1);
}
private final int jjMoveStringLiteralDfa12_0(long old0, long active0, long old1, long active1)
{
   if (((active0 &= old0) | (active1 &= old1)) == 0L)
      return jjStartNfa_0(10, old0, old1); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(11, active0, active1);
      return 12;
   }
   switch(curChar)
   {
      case 97:
         return jjMoveStringLiteralDfa13_0(active0, 0L, active1, 0x1L);
      case 98:
         return jjMoveStringLiteralDfa13_0(active0, 0x600000000000L, active1, 0L);
      case 102:
         return jjMoveStringLiteralDfa13_0(active0, 0x1000000000000000L, active1, 0L);
      case 104:
         if ((active0 & 0x8000000000000000L) != 0L)
            return jjStartNfaWithStates_0(12, 63, 12);
         break;
      case 105:
         if ((active0 & 0x40000000000000L) != 0L)
            return jjStartNfaWithStates_0(12, 54, 12);
         break;
      case 110:
         return jjMoveStringLiteralDfa13_0(active0, 0x400000000L, active1, 0L);
      case 114:
         if ((active1 & 0x2000L) != 0L)
            return jjStartNfaWithStates_0(12, 77, 12);
         return jjMoveStringLiteralDfa13_0(active0, 0x800000000000L, active1, 0L);
      case 115:
         return jjMoveStringLiteralDfa13_0(active0, 0x100000000000L, active1, 0L);
      case 116:
         return jjMoveStringLiteralDfa13_0(active0, 0x2000000000000000L, active1, 0L);
      default :
         break;
   }
   return jjStartNfa_0(11, active0, active1);
}
private final int jjMoveStringLiteralDfa13_0(long old0, long active0, long old1, long active1)
{
   if (((active0 &= old0) | (active1 &= old1)) == 0L)
      return jjStartNfa_0(11, old0, old1); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(12, active0, active1);
      return 13;
   }
   switch(curChar)
   {
      case 45:
         return jjMoveStringLiteralDfa14_0(active0, 0x800000000000L, active1, 0L);
      case 99:
         return jjMoveStringLiteralDfa14_0(active0, 0L, active1, 0x1L);
      case 101:
         return jjMoveStringLiteralDfa14_0(active0, 0x2000100000000000L, active1, 0L);
      case 108:
         return jjMoveStringLiteralDfa14_0(active0, 0x600000000000L, active1, 0L);
      case 111:
         return jjMoveStringLiteralDfa14_0(active0, 0x1000000000000000L, active1, 0L);
      case 115:
         return jjMoveStringLiteralDfa14_0(active0, 0x400000000L, active1, 0L);
      default :
         break;
   }
   return jjStartNfa_0(12, active0, active1);
}
private final int jjMoveStringLiteralDfa14_0(long old0, long active0, long old1, long active1)
{
   if (((active0 &= old0) | (active1 &= old1)) == 0L)
      return jjStartNfa_0(12, old0, old1); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(13, active0, active1);
      return 14;
   }
   switch(curChar)
   {
      case 101:
         if ((active1 & 0x1L) != 0L)
            return jjStartNfaWithStates_0(14, 64, 12);
         break;
      case 105:
         return jjMoveStringLiteralDfa15_0(active0, 0x600000000000L, active1, 0L);
      case 108:
         return jjMoveStringLiteralDfa15_0(active0, 0x100000000000L, active1, 0L);
      case 114:
         if ((active0 & 0x2000000000000000L) != 0L)
            return jjStartNfaWithStates_0(14, 61, 12);
         return jjMoveStringLiteralDfa15_0(active0, 0x1000000000000000L, active1, 0L);
      case 115:
         return jjMoveStringLiteralDfa15_0(active0, 0x800000000000L, active1, 0L);
      case 116:
         return jjMoveStringLiteralDfa15_0(active0, 0x400000000L, active1, 0L);
      default :
         break;
   }
   return jjStartNfa_0(13, active0, active1);
}
private final int jjMoveStringLiteralDfa15_0(long old0, long active0, long old1, long active1)
{
   if (((active0 &= old0) | (active1 &= old1)) == 0L)
      return jjStartNfa_0(13, old0, old1); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(14, active0, 0L);
      return 15;
   }
   switch(curChar)
   {
      case 101:
         if ((active0 & 0x1000000000000000L) != 0L)
            return jjStartNfaWithStates_0(15, 60, 12);
         return jjMoveStringLiteralDfa16_0(active0, 0x800000000000L);
      case 102:
         return jjMoveStringLiteralDfa16_0(active0, 0x100000000000L);
      case 110:
         return jjMoveStringLiteralDfa16_0(active0, 0x600000000000L);
      case 114:
         return jjMoveStringLiteralDfa16_0(active0, 0x400000000L);
      default :
         break;
   }
   return jjStartNfa_0(14, active0, 0L);
}
private final int jjMoveStringLiteralDfa16_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(14, old0, 0L);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(15, active0, 0L);
      return 16;
   }
   switch(curChar)
   {
      case 58:
         return jjMoveStringLiteralDfa17_0(active0, 0x100000000000L);
      case 103:
         return jjMoveStringLiteralDfa17_0(active0, 0x600000000000L);
      case 108:
         return jjMoveStringLiteralDfa17_0(active0, 0x800000000000L);
      case 117:
         return jjMoveStringLiteralDfa17_0(active0, 0x400000000L);
      default :
         break;
   }
   return jjStartNfa_0(15, active0, 0L);
}
private final int jjMoveStringLiteralDfa17_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(15, old0, 0L);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(16, active0, 0L);
      return 17;
   }
   switch(curChar)
   {
      case 58:
         if ((active0 & 0x100000000000L) != 0L)
            return jjStopAtPos(17, 44);
         return jjMoveStringLiteralDfa18_0(active0, 0x600000000000L);
      case 99:
         return jjMoveStringLiteralDfa18_0(active0, 0x400000000L);
      case 102:
         return jjMoveStringLiteralDfa18_0(active0, 0x800000000000L);
      default :
         break;
   }
   return jjStartNfa_0(16, active0, 0L);
}
private final int jjMoveStringLiteralDfa18_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(16, old0, 0L);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(17, active0, 0L);
      return 18;
   }
   switch(curChar)
   {
      case 58:
         if ((active0 & 0x200000000000L) != 0L)
            return jjStopAtPos(18, 45);
         else if ((active0 & 0x400000000000L) != 0L)
            return jjStopAtPos(18, 46);
         return jjMoveStringLiteralDfa19_0(active0, 0x800000000000L);
      case 116:
         return jjMoveStringLiteralDfa19_0(active0, 0x400000000L);
      default :
         break;
   }
   return jjStartNfa_0(17, active0, 0L);
}
private final int jjMoveStringLiteralDfa19_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(17, old0, 0L);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(18, active0, 0L);
      return 19;
   }
   switch(curChar)
   {
      case 58:
         if ((active0 & 0x800000000000L) != 0L)
            return jjStopAtPos(19, 47);
         break;
      case 105:
         return jjMoveStringLiteralDfa20_0(active0, 0x400000000L);
      default :
         break;
   }
   return jjStartNfa_0(18, active0, 0L);
}
private final int jjMoveStringLiteralDfa20_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(18, old0, 0L);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(19, active0, 0L);
      return 20;
   }
   switch(curChar)
   {
      case 111:
         return jjMoveStringLiteralDfa21_0(active0, 0x400000000L);
      default :
         break;
   }
   return jjStartNfa_0(19, active0, 0L);
}
private final int jjMoveStringLiteralDfa21_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(19, old0, 0L);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(20, active0, 0L);
      return 21;
   }
   switch(curChar)
   {
      case 110:
         if ((active0 & 0x400000000L) != 0L)
            return jjStartNfaWithStates_0(21, 34, 12);
         break;
      default :
         break;
   }
   return jjStartNfa_0(20, active0, 0L);
}
private final void jjCheckNAdd(int state)
{
   if (jjrounds[state] != jjround)
   {
      jjstateSet[jjnewStateCnt++] = state;
      jjrounds[state] = jjround;
   }
}
private final void jjAddStates(int start, int end)
{
   do {
      jjstateSet[jjnewStateCnt++] = jjnextStates[start];
   } while (start++ != end);
}
private final void jjCheckNAddTwoStates(int state1, int state2)
{
   jjCheckNAdd(state1);
   jjCheckNAdd(state2);
}
private final void jjCheckNAddStates(int start, int end)
{
   do {
      jjCheckNAdd(jjnextStates[start]);
   } while (start++ != end);
}
private final void jjCheckNAddStates(int start)
{
   jjCheckNAdd(jjnextStates[start]);
   jjCheckNAdd(jjnextStates[start + 1]);
}
static final long[] jjbitVec0 = {
   0xfffffffffffffffeL, 0xffffffffffffffffL, 0xffffffffffffffffL, 0xffffffffffffffffL
};
static final long[] jjbitVec2 = {
   0x0L, 0x0L, 0xffffffffffffffffL, 0xffffffffffffffffL
};
static final long[] jjbitVec3 = {
   0x0L, 0xffffffffffffc000L, 0xfffff0007fffffffL, 0x7fffffL
};
static final long[] jjbitVec4 = {
   0x0L, 0x0L, 0x0L, 0xff7fffffff7fffffL
};
static final long[] jjbitVec5 = {
   0x7ff3ffffffffffffL, 0x7ffffffffffffdfeL, 0xffffffffffffffffL, 0xfc31ffffffffe00fL
};
static final long[] jjbitVec6 = {
   0xffffffL, 0xffffffffffff0000L, 0xf80001ffffffffffL, 0x3L
};
static final long[] jjbitVec7 = {
   0x0L, 0x0L, 0xfffffffbffffd740L, 0xffffd547f7fffL
};
static final long[] jjbitVec8 = {
   0xffffffffffffdffeL, 0xffffffffdffeffffL, 0xffffffffffff0003L, 0x33fcfffffff199fL
};
static final long[] jjbitVec9 = {
   0xfffe000000000000L, 0xfffffffe027fffffL, 0x7fL, 0x707ffffff0000L
};
static final long[] jjbitVec10 = {
   0x7fffffe00000000L, 0xfffe0000000007feL, 0x7cffffffffffffffL, 0x60002f7fffL
};
static final long[] jjbitVec11 = {
   0x23ffffffffffffe0L, 0x3ff000000L, 0x3c5fdfffff99fe0L, 0x30003b0000000L
};
static final long[] jjbitVec12 = {
   0x36dfdfffff987e0L, 0x1c00005e000000L, 0x23edfdfffffbafe0L, 0x100000000L
};
static final long[] jjbitVec13 = {
   0x23cdfdfffff99fe0L, 0x3b0000000L, 0x3bfc718d63dc7e0L, 0x0L
};
static final long[] jjbitVec14 = {
   0x3effdfffffddfe0L, 0x300000000L, 0x3effdfffffddfe0L, 0x340000000L
};
static final long[] jjbitVec15 = {
   0x3fffdfffffddfe0L, 0x300000000L, 0x0L, 0x0L
};
static final long[] jjbitVec16 = {
   0xd7ffffffffffeL, 0x3fL, 0x200d6caefef02596L, 0x1fL
};
static final long[] jjbitVec17 = {
   0x0L, 0x3fffffffeffL, 0x0L, 0x0L
};
static final long[] jjbitVec18 = {
   0x0L, 0x0L, 0xffffffff00000000L, 0x7fffffffff003fL
};
static final long[] jjbitVec19 = {
   0x500000000007daedL, 0x2c62ab82315001L, 0xf580c90040000000L, 0x201080000000007L
};
static final long[] jjbitVec20 = {
   0xffffffffffffffffL, 0xffffffffffffffffL, 0xffffffff0fffffffL, 0x3ffffffffffffffL
};
static final long[] jjbitVec21 = {
   0xffffffff3f3fffffL, 0x3fffffffaaff3f3fL, 0x5fdfffffffffffffL, 0x1fdc1fff0fcf1fdcL
};
static final long[] jjbitVec22 = {
   0x4c4000000000L, 0x0L, 0x7L, 0x0L
};
static final long[] jjbitVec23 = {
   0x3fe00000080L, 0xfffffffffffffffeL, 0xfffffffe001fffffL, 0x7ffffffffffffffL
};
static final long[] jjbitVec24 = {
   0x1fffffffffe0L, 0x0L, 0x0L, 0x0L
};
static final long[] jjbitVec25 = {
   0xffffffffffffffffL, 0xffffffffffffffffL, 0x3fffffffffL, 0x0L
};
static final long[] jjbitVec26 = {
   0xffffffffffffffffL, 0xffffffffffffffffL, 0xfffffffffL, 0x0L
};
static final long[] jjbitVec27 = {
   0x0L, 0x0L, 0x80000000000000L, 0xff7fffffff7fffffL
};
static final long[] jjbitVec28 = {
   0xffffffL, 0xffffffffffff0000L, 0xf80001ffffffffffL, 0x30003L
};
static final long[] jjbitVec29 = {
   0xffffffffffffffffL, 0x30000003fL, 0xfffffffbffffd7c0L, 0xffffd547f7fffL
};
static final long[] jjbitVec30 = {
   0xffffffffffffdffeL, 0xffffffffdffeffffL, 0xffffffffffff007bL, 0x33fcfffffff199fL
};
static final long[] jjbitVec31 = {
   0xfffe000000000000L, 0xfffffffe027fffffL, 0xbbfffffbfffe007fL, 0x707ffffff0016L
};
static final long[] jjbitVec32 = {
   0x7fffffe00000000L, 0xffff03ff0007ffffL, 0x7cffffffffffffffL, 0x3ff3dffffef7fffL
};
static final long[] jjbitVec33 = {
   0xf3ffffffffffffeeL, 0xffcfff1e3fffL, 0xd3c5fdfffff99feeL, 0x3ffcfb080399fL
};
static final long[] jjbitVec34 = {
   0xd36dfdfffff987e4L, 0x1fffc05e003987L, 0xf3edfdfffffbafeeL, 0xffc100003bbfL
};
static final long[] jjbitVec35 = {
   0xf3cdfdfffff99feeL, 0xffc3b0c0398fL, 0xc3bfc718d63dc7ecL, 0xff8000803dc7L
};
static final long[] jjbitVec36 = {
   0xc3effdfffffddfeeL, 0xffc300603ddfL, 0xc3effdfffffddfecL, 0xffc340603ddfL
};
static final long[] jjbitVec37 = {
   0xc3fffdfffffddfecL, 0xffc300803dcfL, 0x0L, 0x0L
};
static final long[] jjbitVec38 = {
   0x7ff7ffffffffffeL, 0x3ff7fffL, 0x3bff6caefef02596L, 0x3ff3f5fL
};
static final long[] jjbitVec39 = {
   0xc2a003ff03000000L, 0xfffe03fffffffeffL, 0x2fe3ffffebf0fdfL, 0x0L
};
static final long[] jjbitVec40 = {
   0x0L, 0x0L, 0x0L, 0x21fff0000L
};
static final long[] jjbitVec41 = {
   0x3efffe000000a0L, 0xfffffffffffffffeL, 0xfffffffe661fffffL, 0x77ffffffffffffffL
};
private final int jjMoveNfa_0(int startState, int curPos)
{
   int[] nextStates;
   int startsAt = 0;
   jjnewStateCnt = 13;
   int i = 1;
   jjstateSet[0] = startState;
   int j, kind = 0x7fffffff;
   for (;;)
   {
      if (++jjround == 0x7fffffff)
         ReInitRounds();
      if (curChar < 64)
      {
         long l = 1L << curChar;
         MatchLoop: do
         {
            switch(jjstateSet[--i])
            {
               case 0:
                  if ((0x3ff000000000000L & l) != 0L)
                  {
                     if (kind > 20)
                        kind = 20;
                     jjCheckNAddTwoStates(6, 7);
                  }
                  else if (curChar == 46)
                     jjCheckNAdd(10);
                  else if (curChar == 39)
                     jjCheckNAddTwoStates(4, 5);
                  else if (curChar == 34)
                     jjCheckNAddTwoStates(1, 2);
                  break;
               case 1:
                  if ((0xfffffffbffffffffL & l) != 0L)
                     jjCheckNAddTwoStates(1, 2);
                  break;
               case 2:
                  if (curChar == 34 && kind > 18)
                     kind = 18;
                  break;
               case 3:
                  if (curChar == 39)
                     jjCheckNAddTwoStates(4, 5);
                  break;
               case 4:
                  if ((0xffffff7fffffffffL & l) != 0L)
                     jjCheckNAddTwoStates(4, 5);
                  break;
               case 5:
                  if (curChar == 39 && kind > 18)
                     kind = 18;
                  break;
               case 6:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 20)
                     kind = 20;
                  jjCheckNAddTwoStates(6, 7);
                  break;
               case 7:
                  if (curChar != 46)
                     break;
                  if (kind > 20)
                     kind = 20;
                  jjCheckNAdd(8);
                  break;
               case 8:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 20)
                     kind = 20;
                  jjCheckNAdd(8);
                  break;
               case 9:
                  if (curChar == 46)
                     jjCheckNAdd(10);
                  break;
               case 10:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 20)
                     kind = 20;
                  jjCheckNAdd(10);
                  break;
               case 12:
                  if ((0x3ff600000000000L & l) == 0L)
                     break;
                  if (kind > 78)
                     kind = 78;
                  jjstateSet[jjnewStateCnt++] = 12;
                  break;
               default : break;
            }
         } while(i != startsAt);
      }
      else if (curChar < 128)
      {
         long l = 1L << (curChar & 077);
         MatchLoop: do
         {
            switch(jjstateSet[--i])
            {
               case 0:
               case 12:
                  if ((0x7fffffe87fffffeL & l) == 0L)
                     break;
                  if (kind > 78)
                     kind = 78;
                  jjCheckNAdd(12);
                  break;
               case 1:
                  jjAddStates(0, 1);
                  break;
               case 4:
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
         MatchLoop: do
         {
            switch(jjstateSet[--i])
            {
               case 0:
                  if (!jjCanMove_1(hiByte, i1, i2, l1, l2))
                     break;
                  if (kind > 78)
                     kind = 78;
                  jjCheckNAdd(12);
                  break;
               case 1:
                  if (jjCanMove_0(hiByte, i1, i2, l1, l2))
                     jjAddStates(0, 1);
                  break;
               case 4:
                  if (jjCanMove_0(hiByte, i1, i2, l1, l2))
                     jjAddStates(2, 3);
                  break;
               case 12:
                  if (!jjCanMove_2(hiByte, i1, i2, l1, l2))
                     break;
                  if (kind > 78)
                     kind = 78;
                  jjCheckNAdd(12);
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
      if ((i = jjnewStateCnt) == (startsAt = 13 - (jjnewStateCnt = startsAt)))
         return curPos;
      try { curChar = input_stream.readChar(); }
      catch(java.io.IOException e) { return curPos; }
   }
}
static final int[] jjnextStates = {
   1, 2, 4, 5, 
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
private static final boolean jjCanMove_1(int hiByte, int i1, int i2, long l1, long l2)
{
   switch(hiByte)
   {
      case 0:
         return ((jjbitVec4[i2] & l2) != 0L);
      case 1:
         return ((jjbitVec5[i2] & l2) != 0L);
      case 2:
         return ((jjbitVec6[i2] & l2) != 0L);
      case 3:
         return ((jjbitVec7[i2] & l2) != 0L);
      case 4:
         return ((jjbitVec8[i2] & l2) != 0L);
      case 5:
         return ((jjbitVec9[i2] & l2) != 0L);
      case 6:
         return ((jjbitVec10[i2] & l2) != 0L);
      case 9:
         return ((jjbitVec11[i2] & l2) != 0L);
      case 10:
         return ((jjbitVec12[i2] & l2) != 0L);
      case 11:
         return ((jjbitVec13[i2] & l2) != 0L);
      case 12:
         return ((jjbitVec14[i2] & l2) != 0L);
      case 13:
         return ((jjbitVec15[i2] & l2) != 0L);
      case 14:
         return ((jjbitVec16[i2] & l2) != 0L);
      case 15:
         return ((jjbitVec17[i2] & l2) != 0L);
      case 16:
         return ((jjbitVec18[i2] & l2) != 0L);
      case 17:
         return ((jjbitVec19[i2] & l2) != 0L);
      case 30:
         return ((jjbitVec20[i2] & l2) != 0L);
      case 31:
         return ((jjbitVec21[i2] & l2) != 0L);
      case 33:
         return ((jjbitVec22[i2] & l2) != 0L);
      case 48:
         return ((jjbitVec23[i2] & l2) != 0L);
      case 49:
         return ((jjbitVec24[i2] & l2) != 0L);
      case 159:
         return ((jjbitVec25[i2] & l2) != 0L);
      case 215:
         return ((jjbitVec26[i2] & l2) != 0L);
      default : 
         if ((jjbitVec3[i1] & l1) != 0L)
            return true;
         return false;
   }
}
private static final boolean jjCanMove_2(int hiByte, int i1, int i2, long l1, long l2)
{
   switch(hiByte)
   {
      case 0:
         return ((jjbitVec27[i2] & l2) != 0L);
      case 1:
         return ((jjbitVec5[i2] & l2) != 0L);
      case 2:
         return ((jjbitVec28[i2] & l2) != 0L);
      case 3:
         return ((jjbitVec29[i2] & l2) != 0L);
      case 4:
         return ((jjbitVec30[i2] & l2) != 0L);
      case 5:
         return ((jjbitVec31[i2] & l2) != 0L);
      case 6:
         return ((jjbitVec32[i2] & l2) != 0L);
      case 9:
         return ((jjbitVec33[i2] & l2) != 0L);
      case 10:
         return ((jjbitVec34[i2] & l2) != 0L);
      case 11:
         return ((jjbitVec35[i2] & l2) != 0L);
      case 12:
         return ((jjbitVec36[i2] & l2) != 0L);
      case 13:
         return ((jjbitVec37[i2] & l2) != 0L);
      case 14:
         return ((jjbitVec38[i2] & l2) != 0L);
      case 15:
         return ((jjbitVec39[i2] & l2) != 0L);
      case 16:
         return ((jjbitVec18[i2] & l2) != 0L);
      case 17:
         return ((jjbitVec19[i2] & l2) != 0L);
      case 30:
         return ((jjbitVec20[i2] & l2) != 0L);
      case 31:
         return ((jjbitVec21[i2] & l2) != 0L);
      case 32:
         return ((jjbitVec40[i2] & l2) != 0L);
      case 33:
         return ((jjbitVec22[i2] & l2) != 0L);
      case 48:
         return ((jjbitVec41[i2] & l2) != 0L);
      case 49:
         return ((jjbitVec24[i2] & l2) != 0L);
      case 159:
         return ((jjbitVec25[i2] & l2) != 0L);
      case 215:
         return ((jjbitVec26[i2] & l2) != 0L);
      default : 
         if ((jjbitVec3[i1] & l1) != 0L)
            return true;
         return false;
   }
}
public static final String[] jjstrLiteralImages = {
"", null, null, null, null, null, "\57", "\57\57", "\174", "\53", "\55", 
"\75", "\41\75", "\74", "\74\75", "\76", "\76\75", "\44", null, null, null, null, 
null, null, null, null, null, "\157\162", "\141\156\144", "\155\157\144", 
"\144\151\166", "\156\157\144\145", "\164\145\170\164", "\143\157\155\155\145\156\164", 
"\160\162\157\143\145\163\163\151\156\147\55\151\156\163\164\162\165\143\164\151\157\156", "\163\145\154\146\72\72", "\143\150\151\154\144\72\72", 
"\160\141\162\145\156\164\72\72", "\141\156\143\145\163\164\157\162\72\72", 
"\141\164\164\162\151\142\165\164\145\72\72", "\156\141\155\145\163\160\141\143\145\72\72", 
"\160\162\145\143\145\144\151\156\147\72\72", "\146\157\154\154\157\167\151\156\147\72\72", 
"\144\145\163\143\145\156\144\141\156\164\72\72", "\141\156\143\145\163\164\157\162\55\157\162\55\163\145\154\146\72\72", 
"\146\157\154\154\157\167\151\156\147\55\163\151\142\154\151\156\147\72\72", "\160\162\145\143\145\144\151\156\147\55\163\151\142\154\151\156\147\72\72", 
"\144\145\163\143\145\156\144\141\156\164\55\157\162\55\163\145\154\146\72\72", "\154\141\163\164", "\160\157\163\151\164\151\157\156", 
"\143\157\165\156\164", "\151\144", "\153\145\171", "\154\157\143\141\154\55\156\141\155\145", 
"\156\141\155\145\163\160\141\143\145\55\165\162\151", "\156\141\155\145", "\163\164\162\151\156\147", "\143\157\156\143\141\164", 
"\163\164\141\162\164\163\55\167\151\164\150", "\143\157\156\164\141\151\156\163", 
"\163\165\142\163\164\162\151\156\147\55\142\145\146\157\162\145", "\163\165\142\163\164\162\151\156\147\55\141\146\164\145\162", 
"\163\165\142\163\164\162\151\156\147", "\163\164\162\151\156\147\55\154\145\156\147\164\150", 
"\156\157\162\155\141\154\151\172\145\55\163\160\141\143\145", "\164\162\141\156\163\154\141\164\145", "\142\157\157\154\145\141\156", 
"\156\157\164", "\164\162\165\145", "\146\141\154\163\145", "\156\165\154\154", 
"\154\141\156\147", "\156\165\155\142\145\162", "\163\165\155", "\146\154\157\157\162", 
"\143\145\151\154\151\156\147", "\162\157\165\156\144", "\146\157\162\155\141\164\55\156\165\155\142\145\162", 
null, "\72", "\50", "\51", "\56", "\56\56", "\133", "\135", "\100", "\54", "\52", };
public static final String[] lexStateNames = {
   "DEFAULT", 
};
static final long[] jjtoToken = {
   0xfffffffff817ffc1L, 0x1ffffffL, 
};
static final long[] jjtoSkip = {
   0x3eL, 0x0L, 
};
protected SimpleCharStream input_stream;
private final int[] jjrounds = new int[13];
private final int[] jjstateSet = new int[26];
protected char curChar;
public XPathParserTokenManager(SimpleCharStream stream)
{
   if (SimpleCharStream.staticFlag)
      throw new Error("ERROR: Cannot use a static CharStream class with a non-static lexical analyzer.");
   input_stream = stream;
}
public XPathParserTokenManager(SimpleCharStream stream, int lexState)
{
   this(stream);
   SwitchTo(lexState);
}
public void ReInit(SimpleCharStream stream)
{
   jjmatchedPos = jjnewStateCnt = 0;
   curLexState = defaultLexState;
   input_stream = stream;
   ReInitRounds();
}
private final void ReInitRounds()
{
   int i;
   jjround = 0x80000001;
   for (i = 13; i-- > 0;)
      jjrounds[i] = 0x80000000;
}
public void ReInit(SimpleCharStream stream, int lexState)
{
   ReInit(stream);
   SwitchTo(lexState);
}
public void SwitchTo(int lexState)
{
   if (lexState >= 1 || lexState < 0)
      throw new TokenMgrError("Error: Ignoring invalid lexical state : " + lexState + ". State unchanged.", TokenMgrError.INVALID_LEXICAL_STATE);
   else
      curLexState = lexState;
}

protected Token jjFillToken()
{
   Token t = Token.newToken(jjmatchedKind);
   t.kind = jjmatchedKind;
   String im = jjstrLiteralImages[jjmatchedKind];
   t.image = (im == null) ? input_stream.GetImage() : im;
   t.beginLine = input_stream.getBeginLine();
   t.beginColumn = input_stream.getBeginColumn();
   t.endLine = input_stream.getEndLine();
   t.endColumn = input_stream.getEndColumn();
   return t;
}

int curLexState = 0;
int defaultLexState = 0;
int jjnewStateCnt;
int jjround;
int jjmatchedPos;
int jjmatchedKind;

public Token getNextToken() 
{
  int kind;
  Token specialToken = null;
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

}
