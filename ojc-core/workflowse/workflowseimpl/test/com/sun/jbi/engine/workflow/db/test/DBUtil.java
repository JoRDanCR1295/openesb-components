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
 * @(#)$Id: DBUtil.java,v 1.2 2010/02/15 19:24:58 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.db.test;

import java.io.IOException;
import java.util.Properties;

public class DBUtil {
    
      public  static Properties getDBProperties () {
        Properties dbProps = null;
        try {
            Properties dbTypeProp = new Properties ();
            dbTypeProp.load(DBUtil.class.getResourceAsStream("dbType"));
            String dbType = dbTypeProp.getProperty("dbType");
            dbProps = new Properties ();
            dbProps.load(DBUtil.class.getResourceAsStream(dbType + ".properties"));
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return dbProps;
    }

}
