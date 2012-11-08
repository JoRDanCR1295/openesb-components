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
 * @(#)FtpHeuristicsFile.java 
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

import java.io.Serializable;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

/**
 * This class is an FTP file representation.
 * @author Harry Liu
 * @version cvs revision:    Last Modified: 
 */
public class FtpHeuristicsFile implements Serializable {
    private static final Logger mLogger =
            Messages.getLogger(FtpHeuristicsFile.class);
    
    public static final int FILE_TYPE = 0;
    public static final int DIRECTORY_TYPE = 1;
    public static final int SYMBOLIC_LINK_TYPE = 2;
    public static final int UNKNOWN_TYPE = 3;
    
    private int type = UNKNOWN_TYPE;
    
    private long size = -1;
    private String rawListing = null;
    
    private String name = null;
    private String link = null;
    
    private boolean dataAvailable = REAL_DATA_AVAILABLE;
    public static final boolean REAL_DATA_AVAILABLE = true;
    public static final boolean REAL_DATA_NOT_AVAILABLE = false;
    
    /**
     * Creates a new FtpHeuristicsFile object.
     */
    public FtpHeuristicsFile() {
    }
    
    /**
     * Sets a raw line from the listing.
     * @param   s  The raw line from the listing.
     */
    public void setRawListing(String s) {
        rawListing = s;
    }
    
    /**
     * Gets a raw line from the listing.
     * @return    The raw line of the listing.
     */
    public String getRawListing() {
        return rawListing;
    }
    
    /**
     * Checks on whether the file is a directory.
     * @return    true if the file is a directory, but false if not.
     */
    public boolean isDirectory() {
        return type == 1;
    }
    
    /**
     * Checks on whether the file is a normal file.
     * @return    true if the file is normal, but false if not.
     */
    public boolean isFile() {
        return type == 0;
    }
    
    /**
     * Checks on whether the file is a symbolic link.
     * @return    true if the file is a symbolic link, but false if not.
     */
    public boolean isSymbolicLink() {
        return type == 2;
    }
    
    /**
     * Checks on whether the file type is unknown.
     * @return    true if the file type is unknown, but false if not.
     */
    public boolean isUnknown() {
        return type == 3;
    }
    
    /**
     * Sets the file type.
     * @param   i  The file type.
     */
    public void setType(int i) {
        type = i;
    }
    
    /**
     * Gets the file type.
     * @return    The file type.
     */
    public int getType() {
        return type;
    }
    
    /**
     * Sets the file name.
     * @param   s  The file name.
     */
    public void setName(String s) {
        name = s;
    }
    
    /**
     * Gets the file name.
     * @return    The file name.
     */
    public String getName() {
        return name;
    }
    
    /**
     * Sets the file size.
     * @param   l  The file size.
     */
    public void setSize(long l) {
        size = l;
    }
    
    /**
     * Gets the file size.
     * @return    The file size.
     */
    public long getSize() {
        return size;
    }
    
    /**
     * Sets the link type.
     * @param   s  The link type.
     */
    public void setLink(String s) {
        link = s;
    }
    
    /**
     * Gets the link type.
     * @return    The link type.
     */
    public String getLink() {
        return link;
    }
    
    /**
     * Returns the raw content for the toString() method.
     * @return   The raw line from the listing.
     */
    public String toString() {
        return rawListing;
    }
    
    /**
     * Checks on the status of the data.
     * @return    The status of the data.
     */
    public boolean isDataAvailable() {
        return dataAvailable;
    }
    
    /**
     * Accessor - Sets the data-available indication to true or false.
     * @param   newDataAvailable : true or false.
     */
    public void setDataAvailable(boolean newDataAvailable) {
        dataAvailable = newDataAvailable;
    }
    
}
