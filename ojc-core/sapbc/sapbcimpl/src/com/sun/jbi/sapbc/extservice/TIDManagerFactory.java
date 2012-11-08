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
 * @(#)TIDManagerFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**************************************************************************
 *
 *          Copyright (c) 2002, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/

package com.sun.jbi.sapbc.extservice;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

/**
 * A factory class to create SAP Transactional ID Managers.
 * 
 * @author Rajesh Dhingra
 * @version 
 */
public class TIDManagerFactory { 
    
    /**
     * Constructs a <code>TIDManagerFactory</code> object 
     * 
     */
    protected TIDManagerFactory() { 
        super();
    }
    
    /**
     * Creates a new instance of the TIDManager
     *
     * @param         connector   The connector supplying the System ID.
     * @param         isServer     To indicate client or server mode
     * @return        A new instance of the TIDManager
     * @throws        Exception    Throws a generic Exception for now           
     */
    public static TIDManager newInstance(TIDConnector aConnector, boolean isServer)throws Exception { 
        //String classname = (isServer ? connector.getServerTidManagerClass() : 
        //                                connector.getClientTidManagerClass());
        
        String classname = "com.stc.connector.sapbapiadapter.sapbapi.FileTIDManagerImpl";
        if (classname != null) { 
            try { 
                Class mgrClass = Class.forName(classname);
                Constructor cons = mgrClass.getConstructor(new Class[] {TIDConnector.class, boolean.class});
                TIDManager mgr = (TIDManager) cons.newInstance(new Object[] {aConnector, new Boolean(isServer)});
                return mgr;
            } catch (ClassNotFoundException e1) { 
                throw new Exception("ClassNotFoundException caught: " + e1.getMessage());
            } catch (NoSuchMethodException e2) { 
                throw new Exception("NoSuchMethodException caught: " + e2.getMessage());
            } catch (SecurityException e3) { 
                throw new Exception("SecurityException caught: " + e3.getMessage());
            } catch (InstantiationException e4) { 
                throw new Exception("InstantiationException caught: " + e4.getMessage());
            } catch (IllegalAccessException e5) { 
                throw new Exception("IllegalAccessException caught: " + e5.getMessage());
            } catch (IllegalArgumentException e6) { 
                throw new Exception("IllegalArgumentException caught: " + e6.getMessage());
            } catch (InvocationTargetException e7) { 
                throw new Exception("InvocationTargetException caught: " + e7.getMessage());
            }
        } else { 
            throw new Exception("TIDManagerFactory: " + " Client TIDManager class not defined");                
        }
    }
}
