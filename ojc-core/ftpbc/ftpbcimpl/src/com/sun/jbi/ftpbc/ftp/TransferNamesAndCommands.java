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
 * @(#)TransferNamesAndCommands.java 
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

import com.sun.jbi.ftpbc.ftp.exception.FtpInterfaceException;
import java.io.Serializable;

/**
 * This class is used to access the resolved transfer names and 
 * transfer commands for Batch Ftp. 
 * @author Harry Liu
 * @version cvs revision:    Last Modified: 
 */

public interface TransferNamesAndCommands extends Serializable {

    /**
     * Gets the resolved target directory name. 
     * This name is resolved from 'Target Directory Name' pattern. 
     * It represents the original working directory name right 
     * before the 'Pre Transfer Command'.
     * @return    The resolved Target Directory Name.
     * @exception    FtpInterfaceException    If some error occurs.
     */
    public String getTargetDirectoryName() throws FtpInterfaceException;
    
    /**
     * Sets the target dir name
     * added for recovery where the target dir name 
     * is a previously recorded known value
     * @param dirName - the resolved target dir
     */
    public void setTargetDirectoryName(String dirName);
    
    /**
     * Gets the resolved target file name. 
     * This name is resolved from 'Target File Name' pattern. 
     * It represents the original working file name right 
     * before the 'Pre Transfer Command'.
     * @return    The resolved Target File Name.
     * @exception    FtpInterfaceException    If some error occurs.
     */
    public String getTargetFileName() throws FtpInterfaceException;
    
    /**
     * Sets the target file name
     * added for recovery where the target file name is 
     * a previously recorded known value
     * @param fileName - the resolved target file
     */
    public void setTargetFileName(String fileName);
    
    /**
     * Gets the Pre Transfer Command.
     * @return    The Pre Transfer Command.
     * @exception    FtpInterfaceException    If some error occurs.
     */
    public String getPreTransferCommand() throws FtpInterfaceException;
    
    /**
     * Gets the resolved pre directory name. 
     * This name is resolved from 'Pre Directory Name' pattern. 
     * It represents the working directory name right 
     * after the 'Pre Transfer Command'.
     * @return    The resolved Pre Directory Name.
     * @exception    FtpInterfaceException    If some error occurs.
     */
    public String getPreDirectoryName() throws FtpInterfaceException;

    /**
     * Sets the pre dir name
     * added for recovery where the pre dir name 
     * is a previously recorded known value
     * @param dirName - the resolved pre dir name
     */
    public void setPreDirectoryName(String dirName);
    
    /**
     * Gets the resolved pre file name. 
     * This name is resolved from 'Pre File Name' pattern. 
     * It represents the working file name right 
     * after the 'Pre Transfer Command'.
     * @return    The resolved Pre File Name.
     * @exception    FtpInterfaceException    If some error occurs.
     */
    public String getPreFileName() throws FtpInterfaceException;

    /**
     * Sets the pre file name
     * added for recovery where the pre file name
     * is a previously recorded known value
     * @param fileName - the resolved pre file name
     */
    public void setPreFileName(String fileName);
    
    /**
     * Gets Post Transfer Command.
     * @return    The Post Transfer Command.
     * @exception    FtpInterfaceException    If some error occurs.
     */
    public String getPostTransferCommand() throws FtpInterfaceException;
    
    /**
     * Gets the resolved post directory name. 
     * This name is resolved from 'Post Directory Name' pattern. 
     * It represents the working directory name right 
     * after the 'Post Transfer Command'.
     * @return    The resolved Post Directory Name.
     * @exception    FtpInterfaceException    If some error occurs.
     */
    public String getPostDirectoryName() throws FtpInterfaceException;

    /**
     * Set the post directory name
     * added for recovery where the post directory name
     * is a previously recorded known value
     * @param dirName - the resolved post dir name
     */
    public void setPostDirectoryName(String dirName);

    /**
     * Gets the resolved post file name. 
     * This is resolved from 'Post File Name' pattern. 
     * It represents the working file name right 
     * after the 'Post Transfer Command'.
     * @return    The resolved Post File Name.
     * @exception    FtpInterfaceException    If some error occurs.
     */
    public String getPostFileName() throws FtpInterfaceException;
    
    /**
     * Set the post file name
     * added for recovery where the post file name
     * is a previously recorded known value
     * @param fileName - the resolved post file name
     */
    public void setPostFileName(String fileName);
    
    /**
     * Checks on whether the outbound transfer is "Append" mode.
     * @return    true or false.
     */
    public boolean getAppend();
    
    /**
     * Resolves Post Transfer Names and Command.
     * The implementation of this method must perform
     * the resolution ONLY once.
     * @exception    FtpInterfaceException    If some error occurs.
     */
    public void resolvePostTransfer() throws FtpInterfaceException;
    
    /**
     * Resolve Pre Transfer Names and Command.
     * The implementation of this method must perform
     * the resolution ONLY once.
     * @exception    FtpInterfaceException    If some error occurs.
     */
    public void resolvePreTransfer() throws FtpInterfaceException;
    
    /**
     * Resolve Target Location Names.
     * The implementation of this method must perform
     * the resolution ONLY once.
     * @exception    FtpInterfaceException    If some error occurs.
     */
    public void resolveTargetLocation() throws FtpInterfaceException;
}
