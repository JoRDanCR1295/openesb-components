/* Generated By:JavaCC: Do not edit this line. SelectorParserTokenManager.java */
using System;
namespace CodeStreet.Selector.Parser
{
	
	public class SelectorParserTokenManager : SelectorParserConstants
	{
		private void  InitBlock()
		{
			//UPGRADE_ISSUE: Class hierarchy differences between 'javax.io.PrintStream' and 'System.IO.StreamWriter' may cause compilation errors. 'ms-help://MS.VSCC.2003/commoner/redir/redirect.htm?keyword="jlca1186_3"'
			System.IO.StreamWriter temp_writer;
			temp_writer = new System.IO.StreamWriter(System.Console.OpenStandardOutput(), System.Console.Out.Encoding);
			temp_writer.AutoFlush = true;
			debugStream = temp_writer;
		}
		//UPGRADE_ISSUE: Class hierarchy differences between 'javax.io.PrintStream' and 'System.IO.StreamWriter' may cause compilation errors. 'ms-help://MS.VSCC.2003/commoner/redir/redirect.htm?keyword="jlca1186_3"'
		virtual public System.IO.StreamWriter DebugStream
		{
			set
			{
				debugStream = value;
			}
			
		}
		virtual public Token NextToken
		{
			get
			{
				int kind;
				Token specialToken = null;
				Token matchedToken;
				int curPos = 0;
				
				//UPGRADE_NOTE: Label 'EOFLoop' was moved. 'ms-help://MS.VSCC.2003/commoner/redir/redirect.htm?keyword="jlca1014_3"'
				for (; ; )
				{
					try
					{
						curChar = input_stream.BeginToken();
						if (curChar == '\0')
						{
							jjmatchedKind = 0;
							matchedToken = jjFillToken();
							return matchedToken;
						}
					}
					catch (System.IO.IOException e)
					{
						jjmatchedKind = 0;
						matchedToken = jjFillToken();
						return matchedToken;
					}
					
					jjmatchedKind = 0x7fffffff;
					jjmatchedPos = 0;
					curPos = jjMoveStringLiteralDfa0_0();
					if (jjmatchedKind != 0x7fffffff)
					{
						if (jjmatchedPos + 1 < curPos)
							input_stream.backup(curPos - jjmatchedPos - 1);
						if ((jjtoToken[jjmatchedKind >> 6] & (1L << (jjmatchedKind & 63))) != 0L)
						{
							matchedToken = jjFillToken();
							return matchedToken;
						}
						else
						{
							//UPGRADE_NOTE: Labeled continue statement was changed to a goto statement. 'ms-help://MS.VSCC.2003/commoner/redir/redirect.htm?keyword="jlca1015_3"'
							goto EOFLoop;
						}
					}
					int error_line = input_stream.EndLine;
					int error_column = input_stream.EndColumn;
					System.String error_after = null;
					bool EOFSeen = false;
					try
					{
						input_stream.readChar(); input_stream.backup(1);
					}
					catch (System.IO.IOException e1)
					{
						EOFSeen = true;
						error_after = curPos <= 1?"":input_stream.GetImage();
						if (curChar == '\n' || curChar == '\r')
						{
							error_line++;
							error_column = 0;
						}
						else
							error_column++;
					}
					if (!EOFSeen)
					{
						input_stream.backup(1);
						error_after = curPos <= 1?"":input_stream.GetImage();
					}
					throw new TokenMgrError(EOFSeen, curLexState, error_line, error_column, error_after, curChar, TokenMgrError.LEXICAL_ERROR);
					//UPGRADE_NOTE: Label 'EOFLoop' was moved. 'ms-help://MS.VSCC.2003/commoner/redir/redirect.htm?keyword="jlca1014_3"'
				EOFLoop: ;
				}
			}
			
		}
		//UPGRADE_NOTE: The initialization of  'debugStream' was moved to method 'InitBlock'. 'ms-help://MS.VSCC.2003/commoner/redir/redirect.htm?keyword="jlca1005_3"'
		public System.IO.StreamWriter debugStream;
		private int jjStopAtPos(int pos, int kind)
		{
			jjmatchedKind = kind;
			jjmatchedPos = pos;
			return pos + 1;
		}
		private int jjMoveStringLiteralDfa0_0()
		{
			switch (curChar)
			{
				
				case (char) (9): 
					jjmatchedKind = 2;
					return jjMoveNfa_0(0, 0);
				
				case (char) (10): 
					jjmatchedKind = 3;
					return jjMoveNfa_0(0, 0);
				
				case (char) (13): 
					jjmatchedKind = 4;
					return jjMoveNfa_0(0, 0);
				
				case (char) (32): 
					jjmatchedKind = 1;
					return jjMoveNfa_0(0, 0);
				
				case (char) (40): 
					jjmatchedKind = 26;
					return jjMoveNfa_0(0, 0);
				
				case (char) (41): 
					jjmatchedKind = 27;
					return jjMoveNfa_0(0, 0);
				
				case (char) (42): 
					jjmatchedKind = 18;
					return jjMoveNfa_0(0, 0);
				
				case (char) (43): 
					jjmatchedKind = 17;
					return jjMoveNfa_0(0, 0);
				
				case (char) (44): 
					jjmatchedKind = 29;
					return jjMoveNfa_0(0, 0);
				
				case (char) (45): 
					jjmatchedKind = 16;
					return jjMoveNfa_0(0, 0);
				
				case (char) (47): 
					jjmatchedKind = 19;
					return jjMoveNfa_0(0, 0);
				
				case (char) (59): 
					jjmatchedKind = 28;
					return jjMoveNfa_0(0, 0);
				
				case (char) (60): 
					jjmatchedKind = 24;
					return jjMoveStringLiteralDfa1_0(0x2200000L);
				
				case (char) (61): 
					jjmatchedKind = 20;
					return jjMoveNfa_0(0, 0);
				
				case (char) (62): 
					jjmatchedKind = 22;
					return jjMoveStringLiteralDfa1_0(0x800000L);
				
				case (char) (65): 
					return jjMoveStringLiteralDfa1_0(0x200L);
				
				case (char) (66): 
					return jjMoveStringLiteralDfa1_0(0x800L);
				
				case (char) (69): 
					return jjMoveStringLiteralDfa1_0(0x8000L);
				
				case (char) (70): 
					return jjMoveStringLiteralDfa1_0(0x40L);
				
				case (char) (73): 
					return jjMoveStringLiteralDfa1_0(0x6000L);
				
				case (char) (76): 
					return jjMoveStringLiteralDfa1_0(0x1000L);
				
				case (char) (78): 
					return jjMoveStringLiteralDfa1_0(0x480L);
				
				case (char) (79): 
					return jjMoveStringLiteralDfa1_0(0x100L);
				
				case (char) (84): 
					return jjMoveStringLiteralDfa1_0(0x20L);
				
				case (char) (97): 
					return jjMoveStringLiteralDfa1_0(0x200L);
				
				case (char) (98): 
					return jjMoveStringLiteralDfa1_0(0x800L);
				
				case (char) (101): 
					return jjMoveStringLiteralDfa1_0(0x8000L);
				
				case (char) (102): 
					return jjMoveStringLiteralDfa1_0(0x40L);
				
				case (char) (105): 
					return jjMoveStringLiteralDfa1_0(0x6000L);
				
				case (char) (108): 
					return jjMoveStringLiteralDfa1_0(0x1000L);
				
				case (char) (110): 
					return jjMoveStringLiteralDfa1_0(0x480L);
				
				case (char) (111): 
					return jjMoveStringLiteralDfa1_0(0x100L);
				
				case (char) (116): 
					return jjMoveStringLiteralDfa1_0(0x20L);
				
				default: 
					return jjMoveNfa_0(0, 0);
				
			}
		}
		private int jjMoveStringLiteralDfa1_0(long active0)
		{
			try
			{
				curChar = input_stream.readChar();
			}
			catch (System.IO.IOException e)
			{
				return jjMoveNfa_0(0, 0);
			}
			switch (curChar)
			{
				
				case (char) (61): 
					if ((active0 & 0x800000L) != 0L)
					{
						jjmatchedKind = 23;
						jjmatchedPos = 1;
					}
					else if ((active0 & 0x2000000L) != 0L)
					{
						jjmatchedKind = 25;
						jjmatchedPos = 1;
					}
					break;
				
				case (char) (62): 
					if ((active0 & 0x200000L) != 0L)
					{
						jjmatchedKind = 21;
						jjmatchedPos = 1;
					}
					break;
				
				case (char) (65): 
					return jjMoveStringLiteralDfa2_0(active0, 0x40L);
				
				case (char) (69): 
					return jjMoveStringLiteralDfa2_0(active0, 0x800L);
				
				case (char) (73): 
					return jjMoveStringLiteralDfa2_0(active0, 0x1000L);
				
				case (char) (78): 
					if ((active0 & 0x2000L) != 0L)
					{
						jjmatchedKind = 13;
						jjmatchedPos = 1;
					}
					return jjMoveStringLiteralDfa2_0(active0, 0x200L);
				
				case (char) (79): 
					return jjMoveStringLiteralDfa2_0(active0, 0x400L);
				
				case (char) (82): 
					if ((active0 & 0x100L) != 0L)
					{
						jjmatchedKind = 8;
						jjmatchedPos = 1;
					}
					return jjMoveStringLiteralDfa2_0(active0, 0x20L);
				
				case (char) (83): 
					if ((active0 & 0x4000L) != 0L)
					{
						jjmatchedKind = 14;
						jjmatchedPos = 1;
					}
					return jjMoveStringLiteralDfa2_0(active0, 0x8000L);
				
				case (char) (85): 
					return jjMoveStringLiteralDfa2_0(active0, 0x80L);
				
				case (char) (97): 
					return jjMoveStringLiteralDfa2_0(active0, 0x40L);
				
				case (char) (101): 
					return jjMoveStringLiteralDfa2_0(active0, 0x800L);
				
				case (char) (105): 
					return jjMoveStringLiteralDfa2_0(active0, 0x1000L);
				
				case (char) (110): 
					if ((active0 & 0x2000L) != 0L)
					{
						jjmatchedKind = 13;
						jjmatchedPos = 1;
					}
					return jjMoveStringLiteralDfa2_0(active0, 0x200L);
				
				case (char) (111): 
					return jjMoveStringLiteralDfa2_0(active0, 0x400L);
				
				case (char) (114): 
					if ((active0 & 0x100L) != 0L)
					{
						jjmatchedKind = 8;
						jjmatchedPos = 1;
					}
					return jjMoveStringLiteralDfa2_0(active0, 0x20L);
				
				case (char) (115): 
					if ((active0 & 0x4000L) != 0L)
					{
						jjmatchedKind = 14;
						jjmatchedPos = 1;
					}
					return jjMoveStringLiteralDfa2_0(active0, 0x8000L);
				
				case (char) (117): 
					return jjMoveStringLiteralDfa2_0(active0, 0x80L);
				
				default: 
					break;
				
			}
			return jjMoveNfa_0(0, 1);
		}
		private int jjMoveStringLiteralDfa2_0(long old0, long active0)
		{
			if (((active0 &= old0)) == 0L)
				return jjMoveNfa_0(0, 1);
			try
			{
				curChar = input_stream.readChar();
			}
			catch (System.IO.IOException e)
			{
				return jjMoveNfa_0(0, 1);
			}
			switch (curChar)
			{
				
				case (char) (67): 
					return jjMoveStringLiteralDfa3_0(active0, 0x8000L);
				
				case (char) (68): 
					if ((active0 & 0x200L) != 0L)
					{
						jjmatchedKind = 9;
						jjmatchedPos = 2;
					}
					break;
				
				case (char) (75): 
					return jjMoveStringLiteralDfa3_0(active0, 0x1000L);
				
				case (char) (76): 
					return jjMoveStringLiteralDfa3_0(active0, 0xc0L);
				
				case (char) (84): 
					if ((active0 & 0x400L) != 0L)
					{
						jjmatchedKind = 10;
						jjmatchedPos = 2;
					}
					return jjMoveStringLiteralDfa3_0(active0, 0x800L);
				
				case (char) (85): 
					return jjMoveStringLiteralDfa3_0(active0, 0x20L);
				
				case (char) (99): 
					return jjMoveStringLiteralDfa3_0(active0, 0x8000L);
				
				case (char) (100): 
					if ((active0 & 0x200L) != 0L)
					{
						jjmatchedKind = 9;
						jjmatchedPos = 2;
					}
					break;
				
				case (char) (107): 
					return jjMoveStringLiteralDfa3_0(active0, 0x1000L);
				
				case (char) (108): 
					return jjMoveStringLiteralDfa3_0(active0, 0xc0L);
				
				case (char) (116): 
					if ((active0 & 0x400L) != 0L)
					{
						jjmatchedKind = 10;
						jjmatchedPos = 2;
					}
					return jjMoveStringLiteralDfa3_0(active0, 0x800L);
				
				case (char) (117): 
					return jjMoveStringLiteralDfa3_0(active0, 0x20L);
				
				default: 
					break;
				
			}
			return jjMoveNfa_0(0, 2);
		}
		private int jjMoveStringLiteralDfa3_0(long old0, long active0)
		{
			if (((active0 &= old0)) == 0L)
				return jjMoveNfa_0(0, 2);
			try
			{
				curChar = input_stream.readChar();
			}
			catch (System.IO.IOException e)
			{
				return jjMoveNfa_0(0, 2);
			}
			switch (curChar)
			{
				
				case (char) (65): 
					return jjMoveStringLiteralDfa4_0(active0, 0x8000L);
				
				case (char) (69): 
					if ((active0 & 0x20L) != 0L)
					{
						jjmatchedKind = 5;
						jjmatchedPos = 3;
					}
					else if ((active0 & 0x1000L) != 0L)
					{
						jjmatchedKind = 12;
						jjmatchedPos = 3;
					}
					break;
				
				case (char) (76): 
					if ((active0 & 0x80L) != 0L)
					{
						jjmatchedKind = 7;
						jjmatchedPos = 3;
					}
					break;
				
				case (char) (83): 
					return jjMoveStringLiteralDfa4_0(active0, 0x40L);
				
				case (char) (87): 
					return jjMoveStringLiteralDfa4_0(active0, 0x800L);
				
				case (char) (97): 
					return jjMoveStringLiteralDfa4_0(active0, 0x8000L);
				
				case (char) (101): 
					if ((active0 & 0x20L) != 0L)
					{
						jjmatchedKind = 5;
						jjmatchedPos = 3;
					}
					else if ((active0 & 0x1000L) != 0L)
					{
						jjmatchedKind = 12;
						jjmatchedPos = 3;
					}
					break;
				
				case (char) (108): 
					if ((active0 & 0x80L) != 0L)
					{
						jjmatchedKind = 7;
						jjmatchedPos = 3;
					}
					break;
				
				case (char) (115): 
					return jjMoveStringLiteralDfa4_0(active0, 0x40L);
				
				case (char) (119): 
					return jjMoveStringLiteralDfa4_0(active0, 0x800L);
				
				default: 
					break;
				
			}
			return jjMoveNfa_0(0, 3);
		}
		private int jjMoveStringLiteralDfa4_0(long old0, long active0)
		{
			if (((active0 &= old0)) == 0L)
				return jjMoveNfa_0(0, 3);
			try
			{
				curChar = input_stream.readChar();
			}
			catch (System.IO.IOException e)
			{
				return jjMoveNfa_0(0, 3);
			}
			switch (curChar)
			{
				
				case (char) (69): 
					if ((active0 & 0x40L) != 0L)
					{
						jjmatchedKind = 6;
						jjmatchedPos = 4;
					}
					return jjMoveStringLiteralDfa5_0(active0, 0x800L);
				
				case (char) (80): 
					return jjMoveStringLiteralDfa5_0(active0, 0x8000L);
				
				case (char) (101): 
					if ((active0 & 0x40L) != 0L)
					{
						jjmatchedKind = 6;
						jjmatchedPos = 4;
					}
					return jjMoveStringLiteralDfa5_0(active0, 0x800L);
				
				case (char) (112): 
					return jjMoveStringLiteralDfa5_0(active0, 0x8000L);
				
				default: 
					break;
				
			}
			return jjMoveNfa_0(0, 4);
		}
		private int jjMoveStringLiteralDfa5_0(long old0, long active0)
		{
			if (((active0 &= old0)) == 0L)
				return jjMoveNfa_0(0, 4);
			try
			{
				curChar = input_stream.readChar();
			}
			catch (System.IO.IOException e)
			{
				return jjMoveNfa_0(0, 4);
			}
			switch (curChar)
			{
				
				case (char) (69): 
					if ((active0 & 0x8000L) != 0L)
					{
						jjmatchedKind = 15;
						jjmatchedPos = 5;
					}
					return jjMoveStringLiteralDfa6_0(active0, 0x800L);
				
				case (char) (101): 
					if ((active0 & 0x8000L) != 0L)
					{
						jjmatchedKind = 15;
						jjmatchedPos = 5;
					}
					return jjMoveStringLiteralDfa6_0(active0, 0x800L);
				
				default: 
					break;
				
			}
			return jjMoveNfa_0(0, 5);
		}
		private int jjMoveStringLiteralDfa6_0(long old0, long active0)
		{
			if (((active0 &= old0)) == 0L)
				return jjMoveNfa_0(0, 5);
			try
			{
				curChar = input_stream.readChar();
			}
			catch (System.IO.IOException e)
			{
				return jjMoveNfa_0(0, 5);
			}
			switch (curChar)
			{
				
				case (char) (78): 
					if ((active0 & 0x800L) != 0L)
					{
						jjmatchedKind = 11;
						jjmatchedPos = 6;
					}
					break;
				
				case (char) (110): 
					if ((active0 & 0x800L) != 0L)
					{
						jjmatchedKind = 11;
						jjmatchedPos = 6;
					}
					break;
				
				default: 
					break;
				
			}
			return jjMoveNfa_0(0, 6);
		}
		private void  jjCheckNAdd(int state)
		{
			if (jjrounds[state] != jjround)
			{
				jjstateSet[jjnewStateCnt++] = state;
				jjrounds[state] = jjround;
			}
		}
		private void  jjAddStates(int start, int end)
		{
			do 
			{
				jjstateSet[jjnewStateCnt++] = jjnextStates[start];
			}
			while (start++ != end);
		}
		private void  jjCheckNAddTwoStates(int state1, int state2)
		{
			jjCheckNAdd(state1);
			jjCheckNAdd(state2);
		}
		private void  jjCheckNAddStates(int start, int end)
		{
			do 
			{
				jjCheckNAdd(jjnextStates[start]);
			}
			while (start++ != end);
		}
		private void  jjCheckNAddStates(int start)
		{
			jjCheckNAdd(jjnextStates[start]);
			jjCheckNAdd(jjnextStates[start + 1]);
		}

		//UPGRADE_NOTE: Final was removed from the declaration of 'jjbitVec0'. 'ms-help://MS.VSCC.2003/commoner/redir/redirect.htm?keyword="jlca1003_3"'
		//UPGRADE_TODO: Literal detected as an unsigned long can generate compilation errors. 'ms-help://MS.VSCC.2003/commoner/redir/redirect.htm?keyword="jlca1175_3"'
		internal static long[] jjbitVec0 = null;

		private int jjMoveNfa_0(int startState, int curPos)
		{
			int strKind = jjmatchedKind;
			int strPos = jjmatchedPos;
			int seenUpto;
			input_stream.backup(seenUpto = curPos + 1);
			try
			{
				curChar = input_stream.readChar();
			}
			catch (System.IO.IOException e)
			{
				throw new System.ApplicationException("Internal Error");
			}
			curPos = 0;
			int[] nextStates;
			int startsAt = 0;
			jjnewStateCnt = 23;
			int i = 1;
			jjstateSet[0] = startState;
			int j, kind = 0x7fffffff;
			for (; ; )
			{
				if (++jjround == 0x7fffffff)
					ReInitRounds();
				if (curChar < 64)
				{
					long l = 1L << (int) curChar;
MatchLoop: 
					do 
					{
						switch (jjstateSet[--i])
						{
							
							case 0: 
								if ((0x3ff000000000000L & l) != 0L)
								{
									if (kind > 30)
										kind = 30;
									jjCheckNAddStates(0, 4);
								}
								else if (curChar == 46)
									jjCheckNAddStates(5, 7);
								else if (curChar == 36)
								{
									if (kind > 34)
										kind = 34;
									jjCheckNAddTwoStates(6, 8);
								}
								else if (curChar == 39)
									jjCheckNAddStates(8, 10);
								break;
							
							case 1: 
								//UPGRADE_TODO: Literal detected as an unsigned long can generate compilation errors. 'ms-help://MS.VSCC.2003/commoner/redir/redirect.htm?keyword="jlca1175_3"'
								if ((unchecked((long) 0xffffff7fffffdbff) & l) != 0L)
									jjCheckNAddStates(8, 10);
								break;
							
							case 2: 
								if (curChar == 39)
									jjCheckNAddStates(8, 10);
								break;
							
							case 3: 
								if (curChar == 39)
									jjstateSet[jjnewStateCnt++] = 2;
								break;
							
							case 4: 
								if (curChar == 39 && kind > 33)
									kind = 33;
								break;
							
							case 5: 
								if (curChar != 36)
									break;
								if (kind > 34)
									kind = 34;
								jjCheckNAddTwoStates(6, 8);
								break;
							
							case 6: 
								if ((0x3ffc01000000000L & l) == 0L)
									break;
								if (kind > 34)
									kind = 34;
								jjCheckNAddTwoStates(6, 8);
								break;
							
							case 7: 
								if (curChar != 58)
									break;
								if (kind > 34)
									kind = 34;
								jjCheckNAddTwoStates(6, 8);
								break;
							
							case 8: 
								if (curChar == 58)
									jjstateSet[jjnewStateCnt++] = 7;
								break;
							
							case 9: 
								if ((0x3ff000000000000L & l) == 0L)
									break;
								if (kind > 30)
									kind = 30;
								jjCheckNAddStates(0, 4);
								break;
							
							case 10: 
								if ((0x3ff000000000000L & l) == 0L)
									break;
								if (kind > 30)
									kind = 30;
								jjCheckNAdd(10);
								break;
							
							case 11: 
								if ((0x3ff000000000000L & l) != 0L)
									jjCheckNAddTwoStates(11, 12);
								break;
							
							case 12: 
								if (curChar != 46)
									break;
								if (kind > 31)
									kind = 31;
								jjCheckNAddTwoStates(13, 14);
								break;
							
							case 13: 
								if ((0x3ff000000000000L & l) == 0L)
									break;
								if (kind > 31)
									kind = 31;
								jjCheckNAddTwoStates(13, 14);
								break;
							
							case 15: 
								if ((0x280000000000L & l) != 0L)
									jjCheckNAdd(16);
								break;
							
							case 16: 
								if ((0x3ff000000000000L & l) == 0L)
									break;
								if (kind > 31)
									kind = 31;
								jjCheckNAdd(16);
								break;
							
							case 17: 
								if ((0x3ff000000000000L & l) != 0L)
									jjCheckNAddTwoStates(17, 18);
								break;
							
							case 18: 
								if (curChar != 46)
									break;
								if (kind > 31)
									kind = 31;
								jjCheckNAdd(19);
								break;
							
							case 19: 
								if ((0x3ff000000000000L & l) == 0L)
									break;
								if (kind > 31)
									kind = 31;
								jjCheckNAdd(19);
								break;
							
							case 20: 
								if (curChar == 46)
									jjCheckNAddStates(5, 7);
								break;
							
							case 21: 
								if ((0x3ff000000000000L & l) == 0L)
									break;
								if (kind > 31)
									kind = 31;
								jjCheckNAddTwoStates(21, 14);
								break;
							
							case 22: 
								if ((0x3ff000000000000L & l) == 0L)
									break;
								if (kind > 31)
									kind = 31;
								jjCheckNAdd(22);
								break;
							
							default:  break;
							
						}
					}
					while (i != startsAt);
				}
				else if (curChar < 128)
				{
					long l = 1L << (curChar & 63);
MatchLoop1: 
					do 
					{
						switch (jjstateSet[--i])
						{
							
							case 0: 
							case 5: 
							case 6: 
								if ((0x7fffffe87fffffeL & l) == 0L)
									break;
								if (kind > 34)
									kind = 34;
								jjCheckNAddTwoStates(6, 8);
								break;
							
							case 1: 
								jjAddStates(8, 10);
								break;
							
							case 14: 
								if ((0x2000000020L & l) != 0L)
									jjAddStates(11, 12);
								break;
							
							default:  break;
							
						}
					}
					while (i != startsAt);
				}
				else
				{
					int i2 = (curChar & 0xff) >> 6;
					long l2 = 1L << (curChar & 63);
MatchLoop1: 
					if (jjbitVec0 == null)
					{
						unchecked
						{
							jjbitVec0 = new long[]{0x0L, 0x0L, (long)0xffffffffffffffff, (long)0xffffffffffffffff};
						}
					}

					do 
					{
						switch (jjstateSet[--i])
						{
							
							case 1: 
								if ((jjbitVec0[i2] & l2) != 0L)
									jjAddStates(8, 10);
								break;
							
							default:  break;
							
						}
					}
					while (i != startsAt);
				}
				if (kind != 0x7fffffff)
				{
					jjmatchedKind = kind;
					jjmatchedPos = curPos;
					kind = 0x7fffffff;
				}
				++curPos;
				if ((i = jjnewStateCnt) == (startsAt = 23 - (jjnewStateCnt = startsAt)))
					break;
				try
				{
					curChar = input_stream.readChar();
				}
				catch (System.IO.IOException e)
				{
					break;
				}
			}
			if (jjmatchedPos > strPos)
				return curPos;
			
			int toRet = System.Math.Max(curPos, seenUpto);
			
			if (curPos < toRet)
				for (i = toRet - System.Math.Min(curPos, seenUpto); i-- > 0; )
					try
					{
						curChar = input_stream.readChar();
					}
					catch (System.IO.IOException e)
					{
						throw new System.ApplicationException("Internal Error : Please send a bug report.");
					}
			
			if (jjmatchedPos < strPos)
			{
				jjmatchedKind = strKind;
				jjmatchedPos = strPos;
			}
			else if (jjmatchedPos == strPos && jjmatchedKind > strKind)
				jjmatchedKind = strKind;
			
			return toRet;
		}
		//UPGRADE_NOTE: Final was removed from the declaration of 'jjnextStates'. 'ms-help://MS.VSCC.2003/commoner/redir/redirect.htm?keyword="jlca1003_3"'
		internal static readonly int[] jjnextStates = new int[]{10, 11, 12, 17, 18, 21, 22, 5, 1, 3, 4, 15, 16};
		//UPGRADE_NOTE: Final was removed from the declaration of 'jjstrLiteralImages'. 'ms-help://MS.VSCC.2003/commoner/redir/redirect.htm?keyword="jlca1003_3"'
		public static readonly System.String[] jjstrLiteralImages = new System.String[]{"", null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, "\x002D", "\x002B", "\x002A", "\x002F", "\x003D", "\x003C\x003E", "\x003E", "\x003E\x003D", "\x003C", "\x003C\x003D", "\x0028", "\x0029", "\x003B", "\x002C", null, null, null, null, null, null, null};
		//UPGRADE_NOTE: Final was removed from the declaration of 'lexStateNames'. 'ms-help://MS.VSCC.2003/commoner/redir/redirect.htm?keyword="jlca1003_3"'
		public static readonly System.String[] lexStateNames = new System.String[]{"DEFAULT"};
		//UPGRADE_NOTE: Final was removed from the declaration of 'jjtoToken'. 'ms-help://MS.VSCC.2003/commoner/redir/redirect.htm?keyword="jlca1003_3"'
		internal static readonly long[] jjtoToken = new long[]{0x6ffffffe1L};
		//UPGRADE_NOTE: Final was removed from the declaration of 'jjtoSkip'. 'ms-help://MS.VSCC.2003/commoner/redir/redirect.htm?keyword="jlca1003_3"'
		internal static readonly long[] jjtoSkip = new long[]{0x1eL};
		private SimpleCharStream input_stream;
		//UPGRADE_NOTE: Final was removed from the declaration of 'jjrounds '. 'ms-help://MS.VSCC.2003/commoner/redir/redirect.htm?keyword="jlca1003_3"'
		private int[] jjrounds = new int[23];
		//UPGRADE_NOTE: Final was removed from the declaration of 'jjstateSet '. 'ms-help://MS.VSCC.2003/commoner/redir/redirect.htm?keyword="jlca1003_3"'
		private int[] jjstateSet = new int[46];
		protected internal char curChar;
		public SelectorParserTokenManager(SimpleCharStream stream)
		{
			InitBlock();
			if (SimpleCharStream.staticFlag)
				throw new System.ApplicationException("ERROR: Cannot use a static CharStream class with a non-static lexical analyzer.");
			input_stream = stream;
		}
		public SelectorParserTokenManager(SimpleCharStream stream, int lexState):this(stream)
		{
			SwitchTo(lexState);
		}
		public virtual void  ReInit(SimpleCharStream stream)
		{
			jjmatchedPos = jjnewStateCnt = 0;
			curLexState = defaultLexState;
			input_stream = stream;
			ReInitRounds();
		}
		private void  ReInitRounds()
		{
			int i;
			jjround = unchecked((int) 0x80000001);
			for (i = 23; i-- > 0; )
				jjrounds[i] = unchecked((int) 0x80000000);
		}
		public virtual void  ReInit(SimpleCharStream stream, int lexState)
		{
			ReInit(stream);
			SwitchTo(lexState);
		}
		public virtual void  SwitchTo(int lexState)
		{
			if (lexState >= 1 || lexState < 0)
				throw new TokenMgrError("Error: Ignoring invalid lexical state : " + lexState + ". State unchanged.", TokenMgrError.INVALID_LEXICAL_STATE);
			else
				curLexState = lexState;
		}
		
		private Token jjFillToken()
		{
			Token t = Token.newToken(jjmatchedKind);
			t.kind = jjmatchedKind;
			System.String im = jjstrLiteralImages[jjmatchedKind];
			t.image = (im == null)?input_stream.GetImage():im;
			t.beginLine = input_stream.BeginLine;
			t.beginColumn = input_stream.BeginColumn;
			t.endLine = input_stream.EndLine;
			t.endColumn = input_stream.EndColumn;
			return t;
		}
		
		internal int curLexState = 0;
		internal int defaultLexState = 0;
		internal int jjnewStateCnt;
		internal int jjround;
		internal int jjmatchedPos;
		internal int jjmatchedKind;
	}
}
