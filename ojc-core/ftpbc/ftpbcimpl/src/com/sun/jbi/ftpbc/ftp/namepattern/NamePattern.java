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
 * @(#)NamePattern.java 
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
package com.sun.jbi.ftpbc.ftp.namepattern;

import com.sun.jbi.ftpbc.Endpoint;
import com.sun.jbi.ftpbc.ftp.exception.FtpInterfaceException;

import com.sun.jbi.internationalization.Messages;

import java.io.IOException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.util.Iterator;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.Arrays;
import java.util.Date;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.text.SimpleDateFormat;

import javax.xml.namespace.QName;

/**
 *  Provides utilities for the ftpbc file-name expansion feature.
 *  You must use the percent symbol (%) to indicate any special character that needs to be expanded.
 *  The characters %% indicate the escaped character %. For example, abc%%d means literal abc%d, and
 *  %d is not expanded again.
 *  <pre>
 *  <p>The ftpbc provides the following types of name expansions:
 *  <p>0. Universally Unique Identifier (%u). It uses java.util.UUID.randomUUID().toString() for the replacement.
 *        For example, abc.%u can be resolved as abc.50c404b3-b3dd-4232-8049-4a41ba9f3016.
 *  <p>1. Time stamps (%[GyMdhHmsSEDFwWakKzZ])+. The ftpbc uses the format of java class java.text.SimpleDateFormat.
 *        For example, abc%y%y%y%y%M%d can be resolved as abc20010625. For more details, please see 
 *        http://java.sun.com/j2se/1.5.0/docs/api/java/text/SimpleDateFormat.html.
 *  <p>2. Sequence number (%[0-9])+, each sequence number counts on independently. 
 *        For example, abc%0 can be resolved as abc1, abc2, abc3, ..., abc100, etc.
 *        The repeated patterns is used for leading zero padding, the number of repeated patterns is 
 *        the minimum number of digits, and shorter numbers are zero-padded to this amount. For example,
 *        abc%1%1%1 can be resolved as abc001, abc002, abc003, ..., abc010, ..., abc100, ..., abc1000, etc.
 *  <p>3. Working file name replacement (%f). Normally, it is used for Rename/Archive operation.
 *        For example, %f.abc will be resolved as working_filename.abc.
 *
 *  The sequence of expansion is 3, 2, 1, 0. Some embedded patterns are possible, for example, working file name may
 *  contain other patterns like time stamps or sequence numbers, all will be resolved.
 *
 *  The same kind of pattern in same input string will be resolved one time as one shot. For example,
 *  abc%9.def%9 will be resolved as abc1.def1 (instead of abc1.def2).
 *
 *  <p>Here are some additional examples:
 *  <p>abc.%y%y%y%y%M%M%d%d.%h%h%m%m%s%s%S%S%S can be resolved as abc.20011112.162532678
 *  <p>abc%0.def%1 can be resolved as abc2.def3
 *  <p>%f.%2 can be resolved as xxxxx.4, xxxxx.5, ... (where xxxxx is the working file name).
 *  <pre>
 *
 *  @author Harry Liu
 *  @author Jim Fu (jfu@sun.com)
 */
// The name can be composed of:
// 1. A = %[GyMdhHmsSEDFwWakKzZ#0123456789fuI], for example, %y, or %M, or %w, etc
// 2. B = any allowed char, for example, A, or B, or V, etc
// 3. C = (A)*(B)*, for example, %y%M%d.abc123, or abc123, or %y%M%d, etc
// 4. D = (C)*, for example, abc%ydef%M123%d456, etc

/*  We use java default time format syntax (US Locale is used),
following comments are from JDK documentation
http://java.sun.com/j2se/1.5.0/docs/api/java/text/SimpleDateFormat.html:

Date and Time Patterns
======================
Date and time formats are specified by date and time pattern strings. 
Within date and time pattern strings, unquoted letters from 'A' to 'Z' 
and from 'a' to 'z' are interpreted as pattern letters representing 
the components of a date or time string. Text can be quoted using 
single quotes (') to avoid interpretation. "''" represents a single quote. 
All other characters are not interpreted; they're simply copied into 
the output string during formatting or matched against the input string during parsing. 

The following pattern letters are defined (all other characters from 'A' to 'Z' 
and from 'a' to 'z' are reserved): 

Letter  Date or Time Component  Presentation        Examples  
======  ======================  ============        ========
G       Era designator          Text                AD  
y       Year                    Year                1996; 96  
M       Month in year           Month               July; Jul; 07  
w       Week in year            Number              27  
W       Week in month           Number              2  
D       Day in year             Number              189  
d       Day in month            Number              10  
F       Day of week in month    Number              2  
E       Day in week             Text                Tuesday; Tue  
a       Am/pm marker            Text                PM  
H       Hour in day (0-23)      Number              0  
k       Hour in day (1-24)      Number              24  
K       Hour in am/pm (0-11)    Number              0  
h       Hour in am/pm (1-12)    Number              12  
m       Minute in hour          Number              30  
s       Second in minute        Number              55  
S       Millisecond             Number              978  
z       Time zone               General time zone   Pacific Standard Time; PST; GMT-08:00  
Z       Time zone               RFC 822 time zone   -0800  

Pattern letters are usually repeated, as their number determines the exact presentation: 

Text:   For formatting, if the number of pattern letters is 4 or more, 
the full form is used; otherwise a short or abbreviated form is used 
if available. For parsing, both forms are accepted, independent of 
the number of pattern letters. 

Number: For formatting, the number of pattern letters is the minimum number of digits, 
and shorter numbers are zero-padded to this amount. For parsing, the number 
of pattern letters is ignored unless it's needed to separate two adjacent fields. 

Year:   For formatting, if the number of pattern letters is 2, the year is truncated 
to 2 digits; otherwise it is interpreted as a number. 

For parsing, if the number of pattern letters is more than 2, the year is 
interpreted literally, regardless of the number of digits. So using the 
pattern "MM/dd/yyyy", "01/11/12" parses to Jan 11, 12 A.D. 

For parsing with the abbreviated year pattern ("y" or "yy"), 
SimpleDateFormat must interpret the abbreviated year relative to some century. 
It does this by adjusting dates to be within 80 years before and 20 years 
after the time the SimpleDateFormat instance is created. For example, 
using a pattern of "MM/dd/yy" and a SimpleDateFormat instance created 
on Jan 1, 1997, the string "01/11/12" would be interpreted as Jan 11, 2012 
while the string "05/04/64" would be interpreted as May 4, 1964. During parsing, 
only strings consisting of exactly two digits, as defined by Character.isDigit(char), 
will be parsed into the default century. Any other numeric string, 
such as a one digit string, a three or more digit string, or a two digit string 
that isn't all digits (for example, "-1"), is interpreted literally. 
So "01/02/3" or "01/02/003" are parsed, using the same pattern, as Jan 2, 3 AD. 
Likewise, "01/02/-3" is parsed as Jan 2, 4 BC. 

Month:  If the number of pattern letters is 3 or more, the month is interpreted as text; 
otherwise, it is interpreted as a number. 

General time zone:  Time zones are interpreted as text if they have names. 
For time zones representing a GMT offset value, 
the following syntax is used: 
GMTOffsetTimeZone:
GMT Sign Hours : Minutes
Sign: one of
+ -
Hours:
Digit
Digit Digit
Minutes:
Digit Digit
Digit: one of
0 1 2 3 4 5 6 7 8 9Hours must be between 0 and 23, 
and Minutes must be between 00 and 59. 
The format is locale independent and digits must 
be taken from the Basic Latin block of the Unicode standard. 

For parsing, RFC 822 time zones are also accepted. 

RFC 822 time zone:  For formatting, the RFC 822 4-digit time zone format is used: 
RFC822TimeZone:
Sign TwoDigitHours Minutes
TwoDigitHours:
Digit DigitTwoDigitHours must be between 00 and 23. 
Other definitions are as for general time zones. 

For parsing, general time zones are also accepted. 

Examples
========
The following examples show how date and time patterns 
are interpreted in the U.S. locale. 
The given date and time are 2001-07-04 12:08:56 local time 
in the U.S. Pacific Time time zone. 
Date and Time Pattern               Result  
=====================               ====== 
"yyyy.MM.dd G 'at' HH:mm:ss z"      2001.07.04 AD at 12:08:56 PDT  
"EEE, MMM d, ''yy"                  Wed, Jul 4, '01  
"h:mm a"                            12:08 PM  
"hh 'o''clock' a, zzzz"             12 o'clock PM, Pacific Daylight Time  
"K:mm a, z"                         0:08 PM, PDT  
"yyyyy.MMMMM.dd GGG hh:mm aaa"      02001.July.04 AD 12:08 PM  
"EEE, d MMM yyyy HH:mm:ss Z"        Wed, 4 Jul 2001 12:08:56 -0700  
"yyMMddHHmmssZ"                     010704120856-0700  
"yyyy-MM-dd'T'HH:mm:ss.SSSZ"        2001-07-04T12:08:56.235-0700  

 */
public class NamePattern {

    private static final Messages mMessages =
            Messages.getMessages(NamePattern.class);
    private static final Logger mLogger =
            Messages.getLogger(NamePattern.class);
    public static final String NAME_PATT_SYM = "%[GyMdhHmsSEDFwWakKzZ0123456789u]";
    public static final Pattern NAME_PATT = Pattern.compile(NAME_PATT_SYM);
    private static final String SEQ_FILE_PREFIX = "sequence";
    private static final String SEQ_LOCK_FILE = "seqlock";
    private static final Pattern SEQ_EXP_ANY = Pattern.compile("%[0-9]");
    private static final Pattern[] SEQ_EXPS = {
        Pattern.compile("(%0)+"),
        Pattern.compile("(%1)+"),
        Pattern.compile("(%2)+"),
        Pattern.compile("(%3)+"),
        Pattern.compile("(%4)+"),
        Pattern.compile("(%5)+"),
        Pattern.compile("(%6)+"),
        Pattern.compile("(%7)+"),
        Pattern.compile("(%8)+"),
        Pattern.compile("(%9)+"),};
    // raw file name pattern
    private String rawFileName = null;
    // original working file name from eGate
    private String fileNameFromEgate = "";
    // starting sequence number
    private long startSeqNo = 1;
    // max sequence number
    private long maxSeqNo = 2147483647;
    // per app persist location
    // use SU path as persist base location
    // sequences are per WSDL operation property
    // save it under <su-path>/op-UUID/sequence0 - 9
    //
    private Endpoint mEP;
    private QName mOpName;

    /**
     * Constructs a new NamePattern object.
     * @param   rawFileName  The file name that needs to be expanded.
     */
    public NamePattern(String rawFileName) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006047.DBG_EXT_FTP_METHOD_CALLED", new Object[]{"NamePattern(String rawFileName)"}));
        }
        this.rawFileName = rawFileName;
        this.fileNameFromEgate = "";
        this.startSeqNo = 1;
        this.maxSeqNo = 2147483647;
    }

    public NamePattern(String rawFileName, Endpoint ep, QName opName) {
        this(rawFileName);
        mEP = ep;
        mOpName = opName;
    }

    /**
     * Used to process the pattern %f (and %p for dir part of path - since in FTPBC, both dir and file are combined).
     * @param       fileName  The file name that needs to be expanded.
     * @return      The expanded file name.
     * @exception   Exception  If some error occurs.
     */
    private String compose(String fileName) throws Exception {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006048.DBG_EXT_FTP_ORIG_FILE_NAME", new Object[]{fileName}));
        }
        //
        //Harry (11/14/2006): a similar pattern %u is introdued and implemented in method expandUUID(),
        //now let's keep %I for a while and use it to match "any name" regardless the guid pattern from matcher side. 
        if (fileName.indexOf("%I") >= 0) {
            Pattern p = Pattern.compile("%I");
            Matcher m = p.matcher(fileName);
            if (m.find()) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006049.DBG_EXT_FTP_REPL_I_IN_NAME", new Object[]{fileName}));
                }
                fileName = m.replaceAll(new RandomGUID(true).toString());
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006050.DBG_EXT_FTP_NAME_AFTER_I_REPL", new Object[]{fileName}));
            }
        }
        // added %p representing current target directory
        if (this.fileNameFromEgate == null || this.fileNameFromEgate.equals("")) {
            if (fileName.indexOf("%f") >= 0 || fileName.indexOf("%p") >= 0) {
                if (mLogger.isLoggable(Level.INFO)) {
                    mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006051.DBG_EXT_FTP_F_NOT_EXPANDED_NO_TARGET"));
                }
            }
            return fileName;
        }
        // in FTPBC, the remote target is given as a path which takes the form of:
        // dir/file, also the pre/post operation target are in the form of: dir/path
        // when %f is used as a substitution of target file, a new %p is used to indicate 
        // the resolved target dir, so the following expression:
        // %p_pre_target/%f.bak
        // when the resolved target dir is my_dir and target file is my_file.dat,
        // will be resolved to my_dir_pre_target/my_file.dat.bak
        // open jbi component issue # 768

        // substitute %f
        Pattern p = Pattern.compile("%f");
        Matcher m = p.matcher(fileName);
        if (m.find()) {
            fileName = m.replaceAll(this.fileNameFromEgate);
        }

        // substitute %p
        p = Pattern.compile("%p");
        m = p.matcher(fileName);
        if (m.find()) {
            fileName = m.replaceAll(this.fileNameFromEgate);
        }

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006052.DBG_EXT_FTP_EXPD_NAME", new Object[]{fileName}));
        }

        return fileName;
    }

    /**
     * This method is used to process UUID pattern.
     * @param       fileName  The file name that needs to be expanded.
     * @return      The expanded file name.
     * @exception   Exception  If some error occurs.
     */
    private String expandUUID(String fileName) throws Exception {
        Pattern expr;
        Matcher match;

        String uuid = UUID.randomUUID().toString();
        expr = Pattern.compile("%u");
        match = expr.matcher(fileName);

        if (match.find()) {
            fileName = match.replaceAll(uuid);
        }

        return fileName;
    }

    /**
     * This method is used to process date/time pattern.
     * @param       fileName  The file name that needs to be expanded.
     * @return      The expanded file name.
     * @exception   Exception  If some error occurs.
     */
    private String expandDateTime(String fileName) throws Exception {
//      RE expr;
//      REMatch match;

        Pattern expr;
        Matcher match;

        Date currentDateTime = new Date();
        //DateFormat formatter = DateFormat.getDateTimeInstance();
        SimpleDateFormat simpleFormatter;
        String before, thisPart, after;

        // for monk, %Y means 2001, %y means 01.
        // for java, yyyy means 2001, yy means 01. So we require put % before every pattern char.
        // for example, %y%y%y%y.%M%M.%d%d means 2001.08.31

        // handle default set of date time patterns
        // from batch eWay doc (monk)
        // expr = new RE("(%[bBcdHIjmMpSUWwxyYZ])+");
        // from java default datetime format syntax

        //expr = new RE("(%[GyMdhHmsSEDFwWakKzZ])+"); // Z is added since jdk1.4
        expr = Pattern.compile("(%[GyMdhHmsSEDFwWakKzZ])+");
        match = expr.matcher(fileName);
        Pattern p = Pattern.compile("%");
        while (match.find()) {
            thisPart = fileName.substring(match.start(), match.end());
            before = fileName.substring(0, match.start());
            after = fileName.substring(match.end());
            //getEndIndex return the next position after the match
            // remove % from thisPart
            Matcher m = p.matcher(thisPart);
            thisPart = m.replaceAll("");
            // expand thisPart using current datatime (thisPart is a pattern)
            //enforce the same locale for both NamePattern and NameMatcher
            // simpleFormatter = new SimpleDateFormat(thisPart);
            simpleFormatter = new SimpleDateFormat(thisPart, DateFormatLocale.LOCALE_IN_USE);
            thisPart = simpleFormatter.format(currentDateTime);
            // compose file name again
            fileName = before + thisPart + after;
            match = expr.matcher(fileName);
        }

        return fileName;
    }

    /**
     * This method is used to process sequence number pattern.
     * @param       fileName  The file name that needs to be expanded.
     * @return      The expanded file name.
     * @exception   Exception  If some error occurs.
     */
    private synchronized String expandSequence(String fileName)
            throws Exception {
        Matcher match = SEQ_EXP_ANY.matcher(fileName);
        if (!match.find()) {
            return fileName;
        }

        char[] zeroChars = new char[256]; // 256 is big enough
        Arrays.fill(zeroChars, '0');
        String allZero = new String(zeroChars);

        long sequenceNo;
        int leadingZeros;

        // locking of the repository:
        // in the file name, there can be one or many
        // references to sequences, e.g.:
        // my_data_folder_%1/my_sub_data_folder_%2/data_%2.dat
        // (1) lock the seqlock
        // (2) expand all the seq refs
        // (3) persist seqs that changed

        if (mEP != null) {
            Vector refs = new Vector();
            for (int i = 0; i < 10; i++) {
                match = SEQ_EXPS[i].matcher(fileName);
                if (match.find()) {
                    refs.add(new Integer(i));
                }
            }

            if (refs.size() > 0) {
                // there is seq ref(s) in the file name
                // needs expansion
                if (mOpName == null) {
                    throw new IllegalArgumentException("Operation name not available when obtain application persistence base dir for the operation.");
                }
                if (mEP.getServiceUnit() == null) {
                    throw new IllegalArgumentException("Service Unit not available when obtain application persistence base dir.");
                }
                String suPath = mEP.getServiceUnit().getServiceUnitPath();
                if (suPath == null) {
                    throw new IllegalArgumentException("Service Unit path not available when obtaining application persistence base dir for operation: " + mOpName.toString());
                }
                String uuid = mEP.getOperationUUID(mOpName);
                File baseDir = new File(suPath, uuid);
                baseDir.mkdir();
                File seqlock = new File(baseDir, SEQ_LOCK_FILE);
                seqlock.createNewFile();
                FileOutputStream fos = new FileOutputStream(seqlock);
                FileChannel fch = null;
                FileLock lock = null;
                try {
                    fch = fos.getChannel();
                    if (fch != null) {
                        lock = fch.lock();
                        long[] seqs = loadSequences(refs, baseDir);
                        for (int i = 0; i < seqs.length; i++) {
                            sequenceNo = seqs[i];
                            Pattern p = SEQ_EXPS[((Integer) refs.get(i)).intValue()];
                            match = p.matcher(fileName);

                            // consider leading zero padding
                            //
                            leadingZeros = 0;
                            while (match.find()) {
                                leadingZeros = (match.end() - match.start()) / 2;
                                leadingZeros = leadingZeros - (String.valueOf(sequenceNo)).length();
                                if (leadingZeros > 0) {
                                    // Need leading zero(s)
                                    fileName = match.replaceFirst(allZero.substring(0, leadingZeros) + String.valueOf(sequenceNo));
                                } else {
                                    fileName = match.replaceFirst(String.valueOf(sequenceNo));
                                }
                                match = p.matcher(fileName);
                            }
                            seqs[i]++;
                        }
                        // save seqs
                        storeSequences(refs, seqs, baseDir);
                    } else {
                        throw new Exception("Failed to obtain channel on file: " + seqlock.getCanonicalPath());
                    }
                } finally {
                    if (lock != null) {
                        lock.release();
                    }
                    if (fch != null) {
                        fch.close();
                    }
                }
            }
        } else {
            throw new Exception("Endpoint not available when obtaining persistence base directory to resolve sequence references.");
        }
        return fileName;

    }

    /**
     * Used to do stand-alone testing.
     * @param       args  Command line parameters.
     * @exception   Exception  If some error occurs.
     */
    public static void main(String args[]) throws Exception {
        if (args.length != 1) {
            System.out.println("Usage: NamePattern pattern");
            return;
        }

        System.out.println("Input pattern is : " + args[0]);
        NamePattern fn = new NamePattern(args[0]);
        fn.setFileNameFromEgate("FileNameFromEGate");
        fn.setStartSeqNo(100);
        System.out.println("Expanded name is : " + fn.expand());
        return;

    }

    /**
     * This method is exposed to other classes for them to call.
     * @return      The expanded file name.
     * @exception   Exception  If some error occurs.
     */
    public String expand() throws Exception {
        Matcher match;
        if (this.rawFileName == null) {
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, mMessages.getString("FTPBC-E006061.ERR_EXT_FTP_NULL_STR_TO_EXPD"));
            }
            throw new Exception(mMessages.getString("FTPBC-E006061.ERR_EXT_FTP_NULL_STR_TO_EXPD"));
        }

        String fileName = this.rawFileName;

        Pattern expr = Pattern.compile("%%");
        match = expr.matcher(fileName);
        if (match.find()) {
            fileName = match.replaceAll("<>");
        }

        fileName = expandUUID(expandDateTime(expandSequence(compose(fileName))));

        expr = Pattern.compile("<>");
        match = expr.matcher(fileName);
        if (match.find()) {
            fileName = match.replaceAll("%");
        }

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006053.DBG_EXT_FTP_RAW_NAME_2_EXPD_NAME", new Object[]{rawFileName, fileName}));
        }

        return fileName;
    }

    /**
     * Gets the max sequence number.
     * @return    The max sequence number.
     */
    public long getMaxSeqNo() {
        return maxSeqNo;
    }

    /**
     * Gets the start sequence number.
     * @return    The start sequence number.
     */
    public long getStartSeqNo() {
        return startSeqNo;
    }

    /**
     * Sets the max sequence number.
     * @param     newMaxSeqNo  The max sequence number.
     */
    public void setMaxSeqNo(long newMaxSeqNo) {
        maxSeqNo = newMaxSeqNo;
    }

    /**
     * Sets the start sequence number.
     * @param     newMaxSeqNo  The start sequence number.
     */
    public void setStartSeqNo(long newStartSeqNo) {
        startSeqNo = newStartSeqNo;
    }

    /**
     * Sets the start sequence number.
     * @param     newFileNameFromEgate  The working file name from eGate.
     */
    public void setFileNameFromEgate(java.lang.String newFileNameFromEgate) {
        fileNameFromEgate = newFileNameFromEgate;
    }

    /**
     * Gets the original working file name (used for pattern %f).
     * @return    The original working file name from eGate.
     */
    public String getFileNameFromEgate() {
        return fileNameFromEgate;
    }

    /**
     * Suppose to be called within a lock protected
     * section, so no need to lock each sequence file
     * concerned.
     * @param seqIdx
     * @return
     * @throws java.io.IOException
     */
    private long[] loadSequences(Vector seqIdx, File baseDir) throws IOException, FtpInterfaceException {
        // read : sequence0, ..., sequence9
        // into array
        long[] result = new long[seqIdx.size()];
        Iterator it = seqIdx.iterator();
        File seqFile = null;
        FileInputStream fis = null;
        byte[] buffer = null;
        int cnt = 0;
        int seqLen = 0;
        Integer index = null;
        while (it.hasNext()) {
            index = (Integer) it.next();
            seqFile = new File(baseDir, SEQ_FILE_PREFIX.concat(index.toString()));
            if (!seqFile.createNewFile()) {
                try {
                    fis = new FileInputStream(seqFile);
                    seqLen = fis.available();
                    if (seqLen > 64) {
                        // redicularly huge seq
                        throw new IllegalArgumentException("sequence exceeds max digits sequence length = " + seqLen + " [max = 64], file :" + seqFile.getCanonicalPath());
                    }
                    // empty sequence treated as seq with init 0
                    if (seqLen > 0) {
                        buffer = new byte[seqLen];
                        fis.read(buffer, 0, seqLen);
                        String seq = new String(buffer);
                        result[cnt] = Long.parseLong(seq);
                    }
                } finally {
                    if (fis != null) {
                        try {
                            fis.close();
                        } catch (Exception e) {
                            // ignore 
                        }
                    }
                }
            } else {
                result[cnt] = 0L;
            }
            cnt++;
        }
        return result;
    }

    private void storeSequences(Vector refs, long[] seqs, File baseDir) {
        if (refs != null && seqs != null && refs.size() == seqs.length) {
            FileOutputStream fos = null;
            try {
                for (int i = 0; i < seqs.length; i++) {
                    fos = new FileOutputStream(new File(baseDir, SEQ_FILE_PREFIX.concat(refs.get(i).toString())));
                    fos.write(("" + seqs[i]).getBytes());
                    fos.close();
                }
            } catch (Exception e) {
                if (fos != null) {
                    try {
                        fos.close();
                    } catch (Exception e1) {
                        // ignore
                        e1.printStackTrace();
                    }
                }
            }
        } else {
            throw new IllegalArgumentException("Invalid parameters when saving sequences");
        }
    }
}
