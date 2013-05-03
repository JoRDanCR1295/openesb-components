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
 * @(#)NameMatcher.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc.ftp.namepattern;

import com.sun.jbi.internationalization.Messages;

import java.text.DateFormatSymbols;
import java.util.Vector;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/*
 * It is used to match names which are formatted/expanded from 
 * class com.sun.jbi.ftpbc.NamePattern. For details about rule/syntax, 
 * please see com.sun.jbi.ftpbc.NamePattern.
 * 
 * 
 * 
 * @author Harry Liu (harry.liu@sun.com)
 *
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.
 */
public class NameMatcher {

    private static final Logger mLogger =
            Messages.getLogger(NameMatcher.class);
    private static Vector patternList = null; // use Vector to ensure the order in key size
    private String rawPattern = null;
    private String workingFileName = null;
    private int flags;
    private Pattern regexPattern = null;
    private Matcher regexMatcher = null;
    // Enables Unix lines mode.
    public static final int UNIX_LINES = 0x01;
    // Enables case-insensitive matching.
    public static final int CASE_INSENSITIVE = Pattern.CASE_INSENSITIVE; //0x02;
    // Permits whitespace and comments in pattern.
    public static final int COMMENTS = Pattern.COMMENTS; //0x04;
    // Enables multiline mode.
    public static final int MULTILINE = Pattern.MULTILINE; //0x08;
    // Enables literal parsing of the pattern.
    public static final int LITERAL = Pattern.LITERAL; //0x10;
    // Enables dotall mode.
    public static final int DOTALL = Pattern.DOTALL; //0x20;
    // Enables Unicode-aware case folding.
    public static final int UNICODE_CASE = Pattern.UNICODE_CASE; //0x40;
    // Enables canonical equivalence.
    public static final int CANON_EQ = Pattern.CANON_EQ; //0x80;

    /** Creates a new instance of NameMatcher 
     * @param rawPattern        The % pattern
     * @param workingFileName   It is used for %f
     * @param flags             Match flags, a bit mask that may include 
     *                          CASE_INSENSITIVE, MULTILINE, DOTALL, UNICODE_CASE, and CANON_EQ 
     */
    public NameMatcher(String rawPattern, String workingFileName, int flags) {
        this.rawPattern = rawPattern; // e.g. file.%d%d
        this.workingFileName = workingFileName;
        this.flags = flags;
    }

    /** Creates a new instance of NameMatcher 
     * @param rawPattern        The % pattern
     * @param workingFileName   It is used for %f
     */
    public NameMatcher(String rawPattern, String workingFileName) {
        this(rawPattern, workingFileName, 0);
    }

    /** Creates a new instance of NameMatcher 
     * @param rawPattern        The % pattern
     * @param flags             Match flags, a bit mask that may include 
     *                          CASE_INSENSITIVE, MULTILINE, DOTALL, UNICODE_CASE, and CANON_EQ 
     */
    public NameMatcher(String rawPattern, int flags) {
        this(rawPattern, null, flags);
    }

    /** Creates a new instance of NameMatcher 
     * @param rawPattern        The % pattern
     */
    public NameMatcher(String rawPattern) { // e.g. file.%d%d
        this(rawPattern, null, 0);
    }

    public String getWorkingFileName() {
        return this.workingFileName;
    }

    public String getRawPattern() {
        return this.rawPattern;
    }

    /*
     * Prepares the pattern list
     */
    //private static void preCompile() {
    static {

        if (null != NameMatcher.patternList &&
                0 < NameMatcher.patternList.size()) {
            //return;
        }

        NameMatcher.patternList = new Vector();

        /*
        // u
        // #, %[2-9]#, %[0-9]
        // f
         **/
        NameMatcher.patternList.add("%I==>.*"); // another guid - "any" name: don't care the pattern when it is generated/matched
        NameMatcher.patternList.add("%u==>[0-9[abcdef]]{8}-[0-9[abcdef]]{4}-[0-9[abcdef]]{4}-[0-9[abcdef]]{4}-[0-9[abcdef]]{12}"); //  from java.util.UUID.randomUUID()
        //NameMatcher.patternList.add("%u==>.*"); // which makes more sense
        /* replaced by %[0-9]
        NameMatcher.patternList.add("%#==>[0-9]{1,}");
        NameMatcher.patternList.add("%2#==>[0-9]{2,}"); // 2 leading zero padding
        NameMatcher.patternList.add("%3#==>[0-9]{3,}"); // 3 leading zero padding
        NameMatcher.patternList.add("%4#==>[0-9]{4,}"); // 4 leading zero padding
        NameMatcher.patternList.add("%5#==>[0-9]{5,}"); // 5 leading zero padding
        NameMatcher.patternList.add("%6#==>[0-9]{6,}"); // 6 leading zero padding
        NameMatcher.patternList.add("%7#==>[0-9]{7,}"); // 7 leading zero padding
        NameMatcher.patternList.add("%8#==>[0-9]{8,}"); // 8 leading zero padding
        NameMatcher.patternList.add("%9#==>[0-9]{9,}"); // 9 leading zero padding
         */
        NameMatcher.patternList.add("%0==>[0-9]{1,}");
        NameMatcher.patternList.add("%0%0==>[0-9]%0");

        NameMatcher.patternList.add("%1==>[0-9]{1,}");
        NameMatcher.patternList.add("%1%1==>[0-9]%1");

        NameMatcher.patternList.add("%2==>[0-9]{1,}");
        NameMatcher.patternList.add("%2%2==>[0-9]%2");

        NameMatcher.patternList.add("%3==>[0-9]{1,}");
        NameMatcher.patternList.add("%3%3==>[0-9]%3");

        NameMatcher.patternList.add("%4==>[0-9]{1,}");
        NameMatcher.patternList.add("%4%4==>[0-9]%4");

        NameMatcher.patternList.add("%5==>[0-9]{1,}");
        NameMatcher.patternList.add("%5%5==>[0-9]%5");

        NameMatcher.patternList.add("%6==>[0-9]{1,}");
        NameMatcher.patternList.add("%6%6==>[0-9]%6");

        NameMatcher.patternList.add("%7==>[0-9]{1,}");
        NameMatcher.patternList.add("%7%7==>[0-9]%7");

        NameMatcher.patternList.add("%8==>[0-9]{1,}");
        NameMatcher.patternList.add("%8%8==>[0-9]%8");

        NameMatcher.patternList.add("%9==>[0-9]{1,}");
        NameMatcher.patternList.add("%9%9==>[0-9]%9");

        //NameMatcher.patternList.add("%f==>"); // to be handled later as instance var instead of static here
        /*
        // GyMdkHmsSEDFwWahKzZ
        G  Era designator  Text  AD  
        y  Year  Year  1996; 96  
        M  Month in year  Month  July; Jul; 07  
        w  Week in year  Number  27  
        W  Week in month  Number  2  
        D  Day in year  Number  189  
        d  Day in month  Number  10  
        F  Day of week in month  Number  2  
        E  Day in week  Text  Tuesday; Tue  
        a  Am/pm marker  Text  PM  
        H  Hour in day (0-23)  Number  0  
        k  Hour in day (1-24)  Number  24  
        K  Hour in am/pm (0-11)  Number  0  
        h  Hour in am/pm (1-12)  Number  12  
        m  Minute in hour  Number  30  
        s  Second in minute  Number  55  
        S  Millisecond  Number  978  
        z  Time zone  General time zone  Pacific Standard Time; PST; GMT-08:00  
        Z  Time zone  RFC 822 time zone  -0800  
         */

        // ensure the generator (NamePattern) and matcher (NameMatcher) are using
        // the same Locale, so the generated file can be understood by matcher.
        DateFormatSymbols symbols = new DateFormatSymbols(DateFormatLocale.LOCALE_IN_USE);

        ////////////////////////////////////////////////
        // keep the sequence to add items!
        ////////////////////////////////////////////////

        NameMatcher.patternList.add("%G==>" + NameMatcher.getGroup(symbols.getEras()));
        //NameMatcher.patternList.add("%G repeats (n>1) times==>(AD|BC)");
        NameMatcher.patternList.add("%G%G==>%G");
        //NameMatcher.patternList.add("%G%G%G%G%G%G==>(AD|BC)");

        NameMatcher.patternList.add("%y==>[0-9]{2}");
        NameMatcher.patternList.add("%y%y==>[0-9]{2}");
        NameMatcher.patternList.add("%y%y%y==>[0-9]{2}");
        NameMatcher.patternList.add("%y%y%y%y==>[0-9]{4}");
        //NameMatcher.patternList.add("%y repeats (n>4) times==>0{n-4}[0-9]{4}"); // zero padding
        NameMatcher.patternList.add("%y%y%y%y%y==>0%y%y%y%y"); // zero padding
        //NameMatcher.patternList.add("%y%y%y%y%y%y==>0{2}[0-9]{4}");

        NameMatcher.patternList.add("%M==>([1-9]|1[012])");
        NameMatcher.patternList.add("%M%M==>(0[1-9]|1[012])");
        NameMatcher.patternList.add("%M%M%M==>" + NameMatcher.getGroup(symbols.getShortMonths()));
        NameMatcher.patternList.add("%M%M%M%M==>" + NameMatcher.getGroup(symbols.getMonths()));
        //NameMatcher.patternList.add("%M repeats (n>4) times==>(January|February|March|April|May|June|July|August|September|October|November|December)");
        NameMatcher.patternList.add("%M%M%M%M%M==>%M%M%M%M");
        //NameMatcher.patternList.add("%M%M%M%M%M%M==>(January|February|March|April|May|June|July|August|September|October|November|December)");

        NameMatcher.patternList.add("%w==>([1-9]|[1-4][0-9]|5[01234])");
        NameMatcher.patternList.add("%w%w==>(0[1-9]|[1-4][0-9]|5[01234])");
        //NameMatcher.patternList.add("%w repeats (n>2) times==>0{n-2}(0[1-9]|[1-4][0-9]|5[01234])");
        NameMatcher.patternList.add("%w%w%w==>0%w%w");
        //NameMatcher.patternList.add("%w%w%w%w%w%w==>0{4}(0[1-9]|[1-4][0-9]|5[01234])");

        // at max - a month can span 6 weeks - ;-)
        NameMatcher.patternList.add("%W==>[1-6]");
        //NameMatcher.patternList.add("%W repeats (n>1) times==>0{n-1}[1-5]");
        NameMatcher.patternList.add("%W%W==>0%W");
        //NameMatcher.patternList.add("%W%W%W%W%W%W==>0{5}[1-5]");


        NameMatcher.patternList.add("%D==>([1-9]|[1-9][0-9]|[12][0-9][0-9]|3[0-5][0-9]|36[0-6])");
        NameMatcher.patternList.add("%D%D==>(0[1-9]|[1-9][0-9]|[12][0-9][0-9]|3[0-5][0-9]|36[0-6])");
        NameMatcher.patternList.add("%D%D%D==>(00[1-9]|0[1-9][0-9]|[12][0-9][0-9]|3[0-5][0-9]|36[0-6])");
        //NameMatcher.patternList.add("%D repeats (n>3) times==>0{n-3}(00[1-9]|0[1-9][0-9]|[12][0-9][0-9]|3[0-5][0-9]|36[0-6])");
        NameMatcher.patternList.add("%D%D%D%D==>0%D%D%D");
        //NameMatcher.patternList.add("%D%D%D%D%D%D==>0{3}(00[1-9]|0[1-9][0-9]|[12][0-9][0-9]|3[0-5][0-9]|36[0-6])");

        NameMatcher.patternList.add("%d==>([1-9]|[12][0-9]|3[01])");
        NameMatcher.patternList.add("%d%d==>(0[1-9]|[12][0-9]|3[01])");
        //NameMatcher.patternList.add("%d repeats (n>2) times==>0{n-2}(0[1-9]|[12][0-9]|3[01])");
        NameMatcher.patternList.add("%d%d%d==>0%d%d");
        //NameMatcher.patternList.add("%d%d%d%d%d%d==>0{4}(0[1-9]|[12][0-9]|3[01])");

        NameMatcher.patternList.add("%F==>[1-5]");
        //NameMatcher.patternList.add("%F repeats (n>1) times==>0{n-1}[1-5]");
        NameMatcher.patternList.add("%F%F==>0%F");
        //NameMatcher.patternList.add("%F%F%F%F%F%F==>0{5}[1-5]");

        NameMatcher.patternList.add("%E==>" + NameMatcher.getGroup(symbols.getShortWeekdays()));
        NameMatcher.patternList.add("%E%E==>" + NameMatcher.getGroup(symbols.getShortWeekdays()));
        NameMatcher.patternList.add("%E%E%E==>" + NameMatcher.getGroup(symbols.getShortWeekdays()));
        NameMatcher.patternList.add("%E%E%E%E==>" + NameMatcher.getGroup(symbols.getWeekdays()));
        //NameMatcher.patternList.add("%E repeats (n>4) times==>(Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)");
        NameMatcher.patternList.add("%E%E%E%E%E==>%E%E%E%E");
        //NameMatcher.patternList.add("%E%E%E%E%E%E==>(Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)");

        NameMatcher.patternList.add("%a==>" + NameMatcher.getGroup(symbols.getAmPmStrings()));
        //NameMatcher.patternList.add("%a repeats (n>1) times==>[AP]M");
        NameMatcher.patternList.add("%a%a==>%a");
        //NameMatcher.patternList.add("%a%a%a%a%a%a==>[AP]M");

        NameMatcher.patternList.add("%H==>([0-9]|1[0-9]|2[0-3])");
        NameMatcher.patternList.add("%H%H==>(0[0-9]|1[0-9]|2[0-3])");
        //NameMatcher.patternList.add("%H repeats (n>2) times==>0{n-2}(0[0-9]|1[0-9]|2[0-3])");
        NameMatcher.patternList.add("%H%H%H==>0%H%H");
        //NameMatcher.patternList.add("%H%H%H%H%H%H==>0{4}(0[0-9]|1[0-9]|2[0-3])");

        NameMatcher.patternList.add("%k==>([1-9]|1[0-9]|2[0-4])");
        NameMatcher.patternList.add("%k%k==>(0[1-9]|1[0-9]|2[0-4])");
        //NameMatcher.patternList.add("%k repeats (n>2) times==>0{n-2}(0[1-9]|1[0-9]|2[0-4])");
        NameMatcher.patternList.add("%k%k%k==>0%k%k");
        //NameMatcher.patternList.add("%k%k%k%k%k%k==>0{4}(0[1-9]|1[0-9]|2[0-4])");

        NameMatcher.patternList.add("%K==>([0-9]|1[01])");
        NameMatcher.patternList.add("%K%K==>(0[0-9]|1[01])");
        //NameMatcher.patternList.add("%K repeats (n>2) times==>0{n-2}(0[0-9]|1[01])");
        NameMatcher.patternList.add("%K%K%K==>0%K%K");
        //NameMatcher.patternList.add("%K%K%K%K%K%K==>0{4}(0[0-9]|1[01])");

        NameMatcher.patternList.add("%h==>([1-9]|1[012])");
        NameMatcher.patternList.add("%h%h==>(0[1-9]|1[012])");
        //NameMatcher.patternList.add("%h repeats (n>2) times==>0{n-2}(0[1-9]|1[012])");
        NameMatcher.patternList.add("%h%h%h==>0%h%h");
        //NameMatcher.patternList.add("%h%h%h%h%h%h==>0{4}(0[1-9]|1[012])");

        NameMatcher.patternList.add("%m==>([0-9]|[1-5][0-9])");
        NameMatcher.patternList.add("%m%m==>[0-5][0-9]");
        //NameMatcher.patternList.add("%m repeats (n>2) times==>0{n-2}[0-5][0-9]");
        NameMatcher.patternList.add("%m%m%m==>0%m%m");
        //NameMatcher.patternList.add("%m%m%m%m%m%m==>0{4}[0-5][0-9]");

        NameMatcher.patternList.add("%s==>([0-9]|[1-5][0-9])");
        NameMatcher.patternList.add("%s%s==>[0-5][0-9]");
        //NameMatcher.patternList.add("%s repeats (n>2) times==>0{n-2}[0-5][0-9]");
        NameMatcher.patternList.add("%s%s%s==>0%s%s");
        //NameMatcher.patternList.add("%s%s%s%s%s%s==>0{4}[0-5][0-9]");

        NameMatcher.patternList.add("%S==>([0-9]|[1-9][0-9]|[1-9][0-9][0-9])");
        NameMatcher.patternList.add("%S%S==>(0[0-9]|[1-9][0-9]|[1-9][0-9][0-9])");
        NameMatcher.patternList.add("%S%S%S==>[0-9]{3}");
        //NameMatcher.patternList.add("%S repeats (n>3) times==>0{n-3}[0-9]{3}");
        NameMatcher.patternList.add("%S%S%S%S==>0%S%S%S");
        //NameMatcher.patternList.add("%S%S%S%S%S%S==>0{3}[0-9]{3}");

        // just put weak restriction on General time zone matching 
        // because the valid General time zone symbols list (2-D) is too big to exhaust :(several thousands)
        NameMatcher.patternList.add("%z==>....?"); // time zone
        NameMatcher.patternList.add("%z%z==>....?");
        NameMatcher.patternList.add("%z%z%z==>....?");
        //NameMatcher.patternList.add("%z repeats (n>3) times==>....+");
        NameMatcher.patternList.add("%z%z%z%z==>.+%z%z%z");
        NameMatcher.patternList.add("%z%z%z%z%z==>%z%z%z%z");
        //NameMatcher.patternList.add("%z%z%z%z%z%z==>....+");

        // DBY : RFC822TimeZone:
        //      Sign (- or +) TwoDigitHours Minutes
        //      TwoDigitHours must be between 00 and 23
        NameMatcher.patternList.add("%Z==>(-|\\\\+)[0-9]{4}");
        //NameMatcher.patternList.add("%Z repeats <n>1) times==>-[0-9]{4}");
        NameMatcher.patternList.add("%Z%Z==>%Z");
        //NameMatcher.patternList.add("%Z%Z%Z%Z%Z%Z==>-[0-9]{4}");

    }

    /*
     * Internally used to join the string arrary into one group.
     */
    private static String getGroup(String[] strings) {
        if (null == strings || 0 == strings.length) {
            return "";
        }

        // starting (
        StringBuffer sb = new StringBuffer("(");

        for (int i = 0; i < strings.length - 1; i++) {
            if (null == strings[i] || 0 == strings[i].length()) {
                continue;
            }
            // append regex_n| 
            sb.append(strings[i]).append("|");

        }

        // the last item in array regex_n)
        if (null == strings[strings.length - 1] ||
                0 == strings[strings.length - 1].length()) {
            // replace the trailing bar | with ) 
            sb.setCharAt(sb.length() - 1, ')');
        } else {
            sb.append(strings[strings.length - 1]).append(")");
        }

        return sb.toString();
    }

    /*
     * Compiles the rawPattern into regexPattern
     */
    public Pattern getRegexPattern() { // e.g. file.[0-9]
        if (null != this.regexPattern) {
            return this.regexPattern;
        }

        String tempPattern = this.rawPattern;
        //? my caller (e.g. FtpFileProvider etc) escapes already?
        tempPattern = this.escapeRegExpChars(tempPattern);

        //if (!tempPattern.contains("%")) {
        if (!Pattern.compile("%[GyMdhHmsSEDFwWakKzZ0-9Iuf]").matcher(tempPattern).find()) {
            // compile tempPattern into regexPattern
            this.regexPattern = Pattern.compile(tempPattern, this.flags);
            return this.regexPattern;
        }

        // (%([IufGyMdhHmsSEDFwWakKzZ]|[2-9]?#|[0-9]))*

        // handle %f here so it doesn't have to be added to static list which would not be safe
        if (null != this.workingFileName && 0 < this.workingFileName.length()) {
            tempPattern = tempPattern.replaceAll("%f", this.workingFileName);
        }

        String entry;
        String key;
        String value;
        int separatorPos;

        // compile rawPattern into regexPattern
        // note the order: for same set of symbols, process the longer one first
        for (int i = NameMatcher.patternList.size() - 1; i >= 0; i--) {
            entry = (String) NameMatcher.patternList.get(i);
            separatorPos = entry.indexOf("==>");
            key = entry.substring(0, separatorPos);
            value = entry.substring(separatorPos + 3);
            if (!value.contains("%")) {
                tempPattern = tempPattern.replaceAll(key, value);
            } else {
                // don't use replaceAll to prevent the case like 
                // %y%y%y%y%y%y%y%y%y%y ==> 0%y%y%y%y0%y%y%y%y, where we expect ==> 000000y%y%y%y
                while (true) {
                    if (tempPattern.replaceFirst(key, value).equals(tempPattern)) {
                        break;
                    }
                    tempPattern = tempPattern.replaceFirst(key, value);
                }
            }
        }

        // compile tempPattern into regexPattern
        this.regexPattern = Pattern.compile(tempPattern, this.flags);
        return this.regexPattern;
    }

    /**
     * Escapes the regular expression special characters in an input string.
     * @param       input  Original input non-escaped string.
     * @return      Escaped output string.
     */
    private String escapeRegExpChars(String input) {
        if (input == null) {
            return null;
        }

        String output = "";
        for (int i = 0; i < input.length(); i++) {
            //String previousChar = (i > 0)? input.substring(i - 1, i) : "";
            String thisChar = input.substring(i, i + 1);
            String nextChar = (i < input.length() - 1) ? input.substring(i + 1, i + 2) : "";
            if ("\\".equals(thisChar)) { // avoid double escapes
                if ("*[]()|+{}:.^$?\\".contains(nextChar) && !"".equals(nextChar)) {
                    // \ is used to escape others: \*, \\ or \. etc
                    output += thisChar + nextChar;
                    i++; //! slip 1 byte
                } else {
                    // \ itself needs to be escaped: \a, \B etc
                    output += "\\" + thisChar;
                }
            } else if ("*[]()|+{}:.^$?".contains(thisChar)) {
                // needs to be escaped: ?, (, * etc
                output += "\\" + thisChar;
            } else {
                // nothing special: other literal, a, B, etc
                output += thisChar;
            }
        }

        return output;
    }

    /*
     * Gets the java.util.regex.Matcher
     */
    public Matcher getRegexMatcher(String input) { // e.g. file.01
        this.regexMatcher = this.getRegexPattern().matcher(input);
        return this.regexMatcher;
    }

    /*
     * Matches the input agaisnt pattern
     */
    public boolean matches(String input) { // e.g. file.01
        return this.getRegexMatcher(input).matches();
    }

    /*
     * Finds the input agaisnt pattern
     */
    public boolean find(String input) { // e.g. file.01
        return this.getRegexMatcher(input).find();
    }

    /*
     * Looks at the input agaisnt pattern
     */
    public boolean lookingAt(String input) { // e.g. file.01
        return this.getRegexMatcher(input).lookingAt();
    }
}
