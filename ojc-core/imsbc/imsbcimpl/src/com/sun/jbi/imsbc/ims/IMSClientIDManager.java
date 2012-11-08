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

package com.sun.jbi.imsbc.ims;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.logging.Logger;
import java.util.logging.Level;

import com.sun.jbi.internationalization.Messages;

public class IMSClientIDManager {

	static final Logger mLogger = Messages.getLogger(IMSClientIDManager.class);
	
	private static final Messages mMessages = Messages.getMessages(IMSClientIDManager.class);

	public static final int IMS_CLIENT_ID_LEN = 8;

    private static final char [] ALPH_NUMERIC_CHARS =
    {'A', 'B', 'C', 'D', 'E', 'F',
     'G', 'H', 'I', 'J', 'K', 'L',
     'M', 'N', 'O', 'P', 'Q', 'R',
     'S', 'T', 'U', 'V', 'W', 'X',
     'Y', 'Z', '0', '1', '2', '3',
     '4', '5', '6', '7', '8', '9'};

    private static List inUseIDs = Collections.synchronizedList(new ArrayList());
    private static Object synchObject = new Object();


    /**
     * Generate a client ID String, of length 8, which is currently not in use.
     * For example, given the pattern String "AB*", this method will return a
     * client ID String which starts with AB and the rest of the String will ,
     * be generated with a number that is not already used.
     *
     * @param clientIDPattern String  The Client ID pattern to use.
     * @return String  The next Client ID randomly generated and is not in used.
     * @throws Exception upon error.
     */
    public static String generateNextClientID(String clientIDPattern)
    throws Exception {
        String candidate = null;

        String prefix = getPrefixString(clientIDPattern);
        int randStrLen = IMS_CLIENT_ID_LEN - prefix.length();
        do {
            // Generate random with new seed to limit clashes.
            Random rand = new Random(System.currentTimeMillis());
            String randStr = random(randStrLen,
                                    0,
                                    0,
                                    true,
                                    true,
                                    null,
                                    rand);
            candidate = prefix + randStr.toUpperCase();
            synchronized(synchObject) {
                if (!isInUse(candidate)) {
                    inUseIDs.add(candidate);
                    break;
                }
            }
        } while (true);

        return candidate;
    }

    public static void collectClientID (String clientID) {
        synchronized(synchObject) {
            if (isInUse(clientID)) {
                inUseIDs.remove(clientID);
            }
        }
    }

    private static String getPrefixString (String clientIDPattern)
    throws Exception {
        String prefix = null;
        int asteriskPosition = clientIDPattern.indexOf("*");
        if (asteriskPosition == -1) {
            throw new Exception(mMessages.getString("IMSBC-E00812.Wrong_Client_Id_Pattern"));
        } else if (asteriskPosition == 0) {
            prefix = "";
        } else {
            prefix = clientIDPattern.substring(0,asteriskPosition);
        }
        return prefix;
    }


    private static String random(int count,
                                 int start,
                                 int end,
                                 boolean letters,
                                 boolean numbers,
                                 char[] chars, Random random)
        throws Exception {
        if (count == 0) {
            return "";
        }
        else if (count < 0) {
            throw new Exception(
                mMessages.getString("IMSBC-E00813.Length_Less_Zero", count));
        }
        if ( (start == 0) && (end == 0)) {
            end = 'z' + 1;
            start = ' ';
            if (!letters && !numbers) {
                start = 0;
                end = Integer.MAX_VALUE;
            }
        }

        StringBuffer buffer = new StringBuffer();
        int gap = end - start;

        while (count-- != 0) {
            char ch;
            if (chars == null) {
                ch = (char) (random.nextInt(gap) + start);
            }
            else {
                ch = chars[random.nextInt(gap) + start];
            }
            if ( (letters && numbers && Character.isLetterOrDigit(ch))
                 || (letters && Character.isLetter(ch))
                 || (numbers && Character.isDigit(ch))
                 || (!letters && !numbers)) {
                buffer.append(ch);
            }
            else {
                count++;
            }
        }
        return buffer.toString();
    }

    private static boolean isInUse (String clientID) {
        synchronized(synchObject) {
            return inUseIDs.contains(clientID);
        }
    }
}
