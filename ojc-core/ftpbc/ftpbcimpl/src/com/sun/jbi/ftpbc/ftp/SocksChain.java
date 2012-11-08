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
 * @(#)SocksChain.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/******************************************************************************
 * Copyright © 2006 Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
 * California 95054, U.S.A. All rights reserved. Sun Microsystems, Inc. has
 * intellectual property rights relating to technology embodied in the product
 * that is described in this document. In particular, and without limitation,
 * these intellectual property rights may include one or more of the U.S. patents
 * listed at http://www.sun.com/patents and one or more additional patents or
 * pending patent applications in the U.S. and in other countries. THIS PRODUCT
 * CONTAINS CONFIDENTIAL INFORMATION AND TRADE SECRETS OF SUN MICROSYSTEMS, INC.
 * USE, DISCLOSURE OR REPRODUCTION IS PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN
 * PERMISSION OF SUN MICROSYSTEMS, INC.U.S. Government Rights - Commercial
 * software.  Government users are subject to the Sun Microsystems, Inc. standard
 * license agreement and applicable provisions of the FAR and its supplements.
 * Use is subject to license terms.  This distribution may include materials
 * developed by third parties. Sun, Sun Microsystems, the Sun logo, Java
 * Composite Application Platform Suite,  SeeBeyond, eGate, eInsight, eVision, eTL,
 * eXchange, eView, eIndex, eBAM and  eWay are trademarks or registered trademarks of
 * Sun Microsystems, Inc. in the U.S. and other countries. All SPARC trademarks are
 * used under license and are trademarks or registered trademarks of SPARC
 * International, Inc. in the U.S. and other countries. Products bearing SPARC
 * trademarks are based upon architecture developed by Sun Microsystems, Inc.
 * UNIX is a registered trademark in the U.S. and other countries, exclusively
 * licensed through X/Open Company, Ltd. This product is covered and controlled by
 * U.S. Export Control laws and may be subject to the export or import laws in
 * other countries.  Nuclear, missile, chemical biological weapons or nuclear
 * maritime end uses or end users, whether direct or indirect, are strictly
 * prohibited.  Export or reexport to countries subject to U.S. embargo or to
 * entities identified on U.S. export exclusion lists, including, but not limited
 * to, the denied persons and specially designated nationals lists is strictly
 * prohibited.
 **/
package com.sun.jbi.ftpbc.ftp;

import java.util.LinkedList;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

/**
 * This class implements a SOCKS chain.
 * Socks A ==> Socks B ==> Socks C ==> ... ==> Socks Z.
 *
 * @author Harry Liu
 * @version cvs revision:    Last Modified: 
 */

public class SocksChain {
    private static final Messages mMessages =
            Messages.getMessages(SocksChain.class);
    private static final Logger mLogger =
            Messages.getLogger(SocksChain.class);
    
    LinkedList list = null;
    
    /**
     * Constructor.
     * Construct a SOCKS chain by taking a SOCKS array as its input.
     * @param  socksList   A list of SOCKS objects.
     * @exception SocksException   If some error occurs.
     */
    public SocksChain(Socks[] socksList) throws SocksException {
        this();
        String msg = null;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"SocksChain.SocksChain(Socks[] socksList)"}));
        }
        
        if (socksList == null) {
            return;
        }
        
        // We have to find out the recursive SOCKS chain and throw exception,
        // so we shouldn't get the whole input directly.
        //this.list.addAll(0, Arrays.asList(socksList));
        
        try {
            for (int i = 0; i < socksList.length; i++){
                this.add(socksList[i]);
            }
        } catch (Exception e) {
            msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {"SocksChain.SocksChain(Socks[] socksList)", e});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg, e);
            }
            throw new SocksException(msg, e);
        }
    }
    
    /**
     * Constructor.
     * Constructs an empty SOCKS chain.
     */
    public SocksChain() {
        this.list = new LinkedList();
    }
    
    /**
     * Appends the specified SOCKS object to the end of the current chain.
     * @param  socks   A SOCKS object.
     * @return true or false.
     * @exception SocksException   If some error occurs.
     */
    public boolean add(Socks socks) throws SocksException {
        String msg = null;
        if (socks == null) {
            msg = mMessages.getString("FTPBC-E006064.ERR_EXT_FTP_NULL_SOCKS_IN_CHAIN", new Object[] {"SocksChain.add(Socks socks)"});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new SocksException(msg);
        }
        if (this.list.contains(socks)) {
            msg = mMessages.getString("FTPBC-E006064.ERR_EXT_FTP_NULL_SOCKS_IN_CHAIN",
                    new Object[] {"SocksChain.add(Socks socks)",
                    socks.getSocksHost(),
                    socks.getSocksPort() 
            });
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new SocksException(msg);
        }
        return this.list.add(socks);
    }
    
    /**
     * Returns the number of SOCKS objects in this SOCKS chain.
     * @return The size of the SOCKS chain.
     */
    public int chainSize() {
        return this.list.size();
    }
    
    /**
     * Returns the SOCKS object at the specified position in the current chain.
     * @param index   The position of SOCKS object.
     * @return   The SOCKS object at the specified position.
     */
    public Socks get(int index) {
        return (Socks)(this.list.get(index));
    }
    
}
