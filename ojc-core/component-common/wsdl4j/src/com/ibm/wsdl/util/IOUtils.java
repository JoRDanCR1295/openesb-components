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
 * @(#)IOUtils.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.ibm.wsdl.util;

import java.io.*;

/**
 * This file is a collection of input/output utilities.
 * 
 * @author   Sanjiva Weerawarana
 * @author   Matthew J. Duftler
 */
public class IOUtils {
  // debug flag - generates debug stuff if true
  static boolean debug = false;

  //////////////////////////////////////////////////////////////////////////

  public static String getStringFromReader (Reader reader) throws IOException {
    BufferedReader bufIn = new BufferedReader(reader);
    StringWriter   swOut = new StringWriter();
    PrintWriter    pwOut = new PrintWriter(swOut);
    String         tempLine;

    while ((tempLine = bufIn.readLine()) != null) {
      pwOut.println(tempLine);
    }

    pwOut.flush();

    return swOut.toString();
  }
}
