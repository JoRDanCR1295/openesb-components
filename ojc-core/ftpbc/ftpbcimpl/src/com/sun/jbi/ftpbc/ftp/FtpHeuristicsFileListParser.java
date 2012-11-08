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
 * @(#)FtpHeuristicsFileListParser.java 
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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Vector;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;
import org.apache.commons.net.ftp.FTPFile;
import org.apache.commons.net.ftp.FTPFileListParser;

/**
 * This class is an implementation of the interface org.apache.commons.net.ftp.FTPFileListParser.
 * @author Harry Liu
 * @version cvs revision:    Last Modified: 
 */
public class FtpHeuristicsFileListParser implements FTPFileListParser {
    private static final Logger mLogger =
            Messages.getLogger(FtpHeuristicsFileListParser.class);
    
    private FtpHeuristics heuristics = null;
    /**
     * Creates a new FtpHeuristicsFileListParser object using the default directory-listing style.
     */
    public FtpHeuristicsFileListParser() throws Exception {
        this(FtpHeuristics.UNIX_DIR_LISTING_STYLE);
    }
    /**
     * Parses an input stream into an FTPFile array.
     * @param       in  An input stream.
     * @return      An FTPFile array.
     * @exception   IOException   If some error occurs.
     */
    // Note: We implement this method defined in interface FTPFileListParser.
    //       Due to limited FtpHeuristics information, not all features in FTPFile
    //       are implemented. But they are enough for our ETD implementation class.
    public FTPFile[] parseFileList(InputStream in) throws IOException {
        return this.parseFileList(in, null);
    }
    
    /**
     * Parses an input stream into an FTPFile array.
     * @param       in  An input stream.
     * @param encoding  The encoding to use.
     * @return      An FTPFile array.
     * @exception   IOException   If some error occurs.
     */
    // Note: We implement this method defined in interface FTPFileListParser.
    //       Due to limited FtpHeuristics information, not all features in FTPFile
    //       are implemented. But they are enough for our ETD implementation class.
    public FTPFile[] parseFileList(InputStream in, String encoding) throws IOException {
        
        //?? Don't use 3rd party Parser implementations - now they expect new usage including FTPClientConfig etc in the new version.
        //?? DefaultFTPFileListParser will not work currectly in batchext - getRawListing() may not return value if DirListingStyle is specified wrongly
        //?? For UNIX, delegate to other parser
        //??if (this.heuristics
        //??        .getDirListingStyle()
        //??        .equals(FtpHeuristics.UNIX_DIR_LISTING_STYLE)) {
        //??    return new DefaultFTPFileListParser().parseFileList(in, encoding);
        //??}
        
        String lines[] = this.heuristics.skipLines(getRawList(in, encoding));
        if (lines == null) {
            return null;
        }
        
        Vector files = new Vector();
        FTPFile aFile = null;
        for (int i = 0; i < lines.length; i++) {
            aFile = parseALine(lines[i]);
            if (aFile != null) {
                files.addElement(aFile);
            }
        }
        
        // clean up and finish
        if (files.size() == 0) {
            return null;
        } else {
            FTPFile[] f = new FTPFile[files.size()];
            files.copyInto(f);
            return f;
        }
        
    }
    
    /**
     * Creates a new FtpHeuristicsFileListParser object using the specified directory-listing style.
     * @param   dirListingStyle The directory-listing style.
     */
    public FtpHeuristicsFileListParser(String dirListingStyle)
    throws Exception {
        this(new FtpHeuristics(dirListingStyle));
    }
    
    /**
     * Converts the input stream into a string array (one entry per line).
     * @param       in  An input stream from a socket.
     * @param encoding  The encoding to use.
     * @return      A string array.
     * @exception   IOException   If some error occurs.
     */
    private String[] getRawList(InputStream in, String encoding) throws IOException {
        BufferedReader reader = null;
        // The following hard-coded checking may not be needed
        // if we configure/accept it via _ControlEncoding_
        if (this.heuristics.getDirListingStyle().equals(
                FtpHeuristics.EUC_JP_UNIX_DIR_LISTING_STYLE)) {
            
            reader = new BufferedReader(new InputStreamReader(
                    in, FtpFileConfigConstants.ENCODING_EUC_JP));
        } else if (this.heuristics.getDirListingStyle().equals(
                FtpHeuristics.SJIS_UNIX_DIR_LISTING_STYLE)) {
            reader = new BufferedReader(new InputStreamReader(
                    in, FtpFileConfigConstants.ENCODING_SJIS));
        } else {
            reader = new BufferedReader(
                    new InputStreamReader(in, encoding));
        }
        Vector lists = new Vector();
        String aLine = reader.readLine();
        for (; aLine != null; aLine = reader.readLine()) {
            lists.addElement(aLine);
        }
        reader.close();
        if (lists.size() == 0)
            return null;
        else {
            String[] s = new String[lists.size()];
            lists.copyInto(s);
            return s;
        }
    }
    
    /**
     * Parses a string line into an FTPFile object.
     * @param       aLine  A string line.
     * @return      An FTPFile object.
     */
    // Note: Due to limited FtpHeuristics information, not all features in FTPFile
    //       are implemented. But they are enough for our ETD implementation class.
    private FTPFile parseALine(String aLine) {
        if (aLine == null || aLine.trim().equals("")) {
            // putlog - Invalid input string (skipped).
            return null;
        }
        
        /* In FtpHeristics.cfg, no information for following FTPFile properties:
         
           hardLinkCount
           user
           group
           timestamp
           permissions
         
           So no coresponding get methods can be used.
           Solutions: 1. change FtpHeuristics.cfg based on class FTPFile
                      2. use a different class FTPHeristicsFile based on FtpHeuristics.cfg
                         (cannot be extended from class FTPFile). And / or
                         The new FTPClient list methods that return types are FTPFile will
                         return FTPHeuristicsFile.
                      3. Don't use these get methods of FTPFile.
         
         */
        
        FtpHeuristicsFile aHeuristicsFile = this.heuristics.parseALine(aLine);
        
        if (aHeuristicsFile == null) {
            return null;
        }
        
        // Convert FTPHeuristicsFile to FTPFile
        FTPFile aFile = new FTPFile();
        aFile.setRawListing(aHeuristicsFile.getRawListing());
        aFile.setType(aHeuristicsFile.getType());
        aFile.setLink(aHeuristicsFile.getLink());
        aFile.setName(aHeuristicsFile.getName());
        aFile.setSize(aHeuristicsFile.getSize());
        
        return aFile;
    }
    
    /**
     * Creates a new FtpHeuristicsFileListParser object using the specified directory-listing style.
     * @param   heuristics The FtpHeuristics instance.
     */
    public FtpHeuristicsFileListParser(FtpHeuristics heuristics)
    throws Exception {
        this.heuristics = heuristics;
    }
}
