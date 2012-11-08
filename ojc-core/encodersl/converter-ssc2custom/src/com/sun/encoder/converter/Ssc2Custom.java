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
 * @(#)Ssc2Custom.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.converter;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import java.math.BigInteger;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.XmlException;
import org.apache.xmlbeans.XmlObject;
import org.apache.xmlbeans.impl.xb.xsdschema.Element;
import org.apache.xmlbeans.impl.xb.xsdschema.ExplicitGroup;
import org.apache.xmlbeans.impl.xb.xsdschema.FormChoice;
import org.apache.xmlbeans.impl.xb.xsdschema.LocalComplexType;
import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument;
import org.apache.xmlbeans.impl.xb.xsdschema.TopLevelElement;
import org.apache.xmlbeans.impl.xb.xsdschema.AnnotationDocument.Annotation;
import org.apache.xmlbeans.impl.xb.xsdschema.IncludeDocument.Include;
import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument.Schema;

import com.sun.encoder.custom.appinfo.CustomEncoding;
import com.sun.encoder.custom.appinfo.Delimiter;
import com.sun.encoder.custom.appinfo.DelimiterBytes;
import com.sun.encoder.custom.appinfo.DelimiterLevel;
import com.sun.encoder.custom.appinfo.DelimiterSet;
import com.sun.encoder.custom.appinfo.EmbeddedBytes;
import com.sun.encoder.custom.appinfo.NodeProperties;
import com.sun.encoder.custom.appinfo.Delimiter.Kind;
import com.sun.encoder.custom.appinfo.Delimiter.OptionalMode;
import com.sun.encoder.custom.appinfo.Delimiter.Origin;
import com.sun.encoder.custom.appinfo.Delimiter.TerminatorMode;
import com.sun.encoder.custom.appinfo.NodeProperties.Alignment;
import com.sun.encoder.custom.appinfo.NodeProperties.NOfN;
import com.sun.encoder.custom.appinfo.NodeProperties.NodeType;
import com.sun.encoder.custom.appinfo.NodeProperties.Order;
import com.sun.encoder.custom.appinfo.NodeProperties.Scvngr;
import com.sun.encoder.custom.appinfo.NodeProperties.UndefDataPolicy;
import com.sun.encoder.frmwk.appinfo.EncodingMark;
import com.sun.encoder.frmwk.appinfo.EncodingMark.CreatedBy;
import com.sun.encoder.frmwk.appinfo.EncodingMark.Encoding;
import com.sun.encoder.frmwk.appinfo.EncodingMark.CreatedBy.Version;
import com.sun.encoder.runtime.provider.Misc;
import com.sun.stc.jcs.ssc.GenSsc;
import com.sun.stc.jcs.ssc.GenXsc;
import com.sun.stc.jcs.ssc.Monk.MonkBoolean;
import com.sun.stc.jcs.ssc.Monk.MonkData;
import com.sun.stc.jcs.ssc.Monk.MonkInteger;
import com.sun.stc.jcs.ssc.Monk.MonkLoc;
import com.sun.stc.jcs.ssc.Monk.MonkPair;
import com.sun.stc.jcs.ssc.Monk.MonkString;
import com.sun.stc.jcs.ssc.Monk.MonkSymbol;
import com.sun.stc.jcs.ssc.Monk.MonkLexer;
import com.sun.stc.jcs.ssc.Monk.MonkNil;
import com.sun.stc.jcsre.JCSProperties;
import com.sun.stc.jcsre.StringCoderFactory;
import com.sun.stc.jcsre.Option;

/**
 * Class for the conversion of SSC to Custom Encoder definition.
 */
public class Ssc2Custom {
    
    public static final String ENCODING_NS_URI = "urn:com.sun:encoder";
    public static final String CUSTOM_ENCODING_NAME = "Custom Encoding";
    public static final String CUSTOM_ENCODING_NS_URI =
        "urn:com.sun:encoder-custom-1.0";
    public static final String CUSTOM_ENCODING_STYLE = "customencoder-1.0";
    
    public static boolean debug = false;

    /**
     * Flag to allow safe spaces in normal-safe escaped attribute strings. When
     * set, will leave single embedded spaces as spaces in the XSC file, because
     * it will not be affected by XML attribute normalization. Initialized from
     * the boolean JCS property "SscConv.space".
     */
    static boolean showSpace = JCSProperties.getFlag("SscConv.space", true);

    /**
     * Flag to force output of the "defaultBytes" attribute for each
     * &lt;node&gt; entitiy in the XSC file that also has a "defaultValue"
     * attribute value. Used to check that the preservation of the encoding is
     * working correctly. Initialized from the boolean JCS property
     * "SscConv.bytes".
     */
    public boolean defBytes = JCSProperties.getFlag("SscConv.bytes", false);
    
    // Root of SSC tree; used for relative search path in references.
    private File sscInputRoot = null;

    /*
     * Ordered list of directories relative to sscInputRoot, comprising the
     * search path to resolve global template references in input SSC files.
     */
    private String[] sscSearch = null;

    // List of load-path mappings (original, normalized).
    private Map<String, String> loads = null;

    // target namespace of the encoder definition XSD
    private String targetNamespace;

    private MonkMatch mdes = null;

    private MonkMatch cdes, ldes, ddes;
    
    private String sscEncoding = null;
    
    private Set<NoSupport> notSupported = new HashSet<NoSupport>();

    /**
     * Registers the common conversion context.
     * 
     * @param idir input directory of SSC files
     * @param search Monk load-path, directories relative to idir
     * @param targetNamespace target namespace of the encoder definition XSD
     */
    Ssc2Custom(File idir, String[] search, String targetNamespace) {
        this.sscInputRoot = idir;
        this.sscSearch = search;
        this.targetNamespace = (targetNamespace == "" ? null : targetNamespace);
    }
    
    /**
     * Converts path using backslash to path using forward slash.
     * @param f the file path
     * @return the path using forward slash
     */
    public static String slashPath (File f) {
        return f.getPath().replace('\\', '/');
    }

    /**
     * Compares two MonkData values for content equivalence. This has only been
     * implemented for a few primitive types; all non-primitive types are
     * compared for object identity. Null values are accepted, they are only
     * equivalent to null.
     * 
     * @param m1
     *            1st Monk data item (or null)
     * @param m2
     *            2nd Monk data item (or null)
     * @return the test result
     */
    public static boolean monkEquals(MonkData m1, MonkData m2) {
        if (m1 == null)
            return (m2 == null);
        if (m2 == null)
            return false;
        if (m1.type != m2.type)
            return false;
        switch (m1.type) {
        case MonkData.BOOLEAN:
            MonkBoolean mb1 = (MonkBoolean) m1;
            MonkBoolean mb2 = (MonkBoolean) m2;
            return (mb1.value == mb2.value);

        case MonkData.INTEGER:
            MonkInteger mi1 = (MonkInteger) m1;
            MonkInteger mi2 = (MonkInteger) m2;
            return (mi1.exact == mi2.exact) && (mi1.value == mi2.value);

        case MonkData.STRING:
            MonkString ms1 = (MonkString) m1;
            MonkString ms2 = (MonkString) m2;
            if (ms1.length != ms2.length)
                return false;
            for (int i = 0; i < ms1.length; i++)
                if (ms1.value[i] != ms2.value[i])
                    return false;
            return true;

        case MonkData.SYMBOL:
            return m1.getSymbol().equals(m2.getSymbol());
        }
        return false;
    }

    /**
     * Converts output data character to escaped XML string. This uses the
     * so-called "normal-safe" XSC format, in which anything other that
     * printable ASCII is rendered as backslash + "u" + 4-digit hex code. This
     * form is impervious to attribute normalisation, but does require
     * encoding/decoding.
     * 
     * @param sb
     *            buffer to emit string
     * @param c
     *            Unicode character value
     */
    public static void sscNormal(StringBuffer sb, int c) {
        boolean esc = false;
        switch (c) {
        case '"':
        case '<':
        case '>':
        case '&':
        case '\'':
        case '\\':
            esc = true;
            break;
        default:
            esc = (c < 0x21 || 0x7E < c);
        }
        if (esc) {
            sb.append('\\');
            sb.append('u');
            numOut(sb, c, 4, 16);
        } else {
            sb.append((char) c);
        }
    }

    /**
     * Converts output data character to escaped XML string. Special characters
     * are protected by using character-entity references or XML specials.
     * 
     * @param sb
     *            buffer to emit string
     * @param c
     *            Unicode character value
     */
    public static void sscEscape(StringBuffer sb, int c) {
        switch (c) {
        case '<':
            sb.append("&lt;");
            break;
        case '>':
            sb.append("&gt;");
            break;
        case '&':
            sb.append("&amp;");
            break;
        case '"':
            sb.append("&quot;");
            break;
        case '\'':
            sb.append("&quot;");
            break;
        default:
            if (c < 0x21 || 0x7E < c) {
                sb.append("&#x");
                numOut(sb, c, 4, 16);
                sb.append(";");
            } else
                sb.append((char) c);
        }
    }

    // Convert output Monk string to normal-safe XML string.
    public static void sscNormal(StringBuffer sb, MonkString s) {
        for (int i = 0; i < s.length; i++)
            sscNormal(sb, s.value[i]);
    }

    // Convert output Monk string to normal-safe XML string.
    public static String sscNormal(MonkString s) {
        StringBuffer sb = new StringBuffer();
        sscNormal(sb, s);
        return sb.toString();
    }

    // Convert output Monk string to escaped XML string.
    public static void sscEscape(StringBuffer sb, MonkString s) {
        for (int i = 0; i < s.length; i++)
            sscEscape(sb, s.value[i]);
    }

    // Convert output data string to normal-safe XML string.
    public static void sscNormal(StringBuffer sb, String s) {
        int last = s.length() - 1;
        if (showSpace) {
            // Special improvement mode: show single spaces as such in output.
            boolean spc = true;
            for (int i = 0; i < last; i++) {
                char c = s.charAt(i);
                if (!spc && c == ' ') {
                    sb.append(' ');
                    spc = true;
                } else {
                    if (c == '\\' && s.charAt(i + 1) == 'u') {
                        sb.append(c);
                        spc = false;
                    } else {
                        sscNormal(sb, c);
                        spc = false;
                    }
                }
            }
            if (last >= 0)
                sscNormal(sb, s.charAt(last));
        } else {
            // Regular brand.
            for (int i = 0; i <= last; i++)
                sscNormal(sb, s.charAt(i));
        }
    }

    // Convert output data string to normal-safe XML string.
    public static String sscNormal(String s) {
        StringBuffer sb = new StringBuffer();
        sscNormal(sb, s);
        return sb.toString();
    }

    // Convert output data string to escaped XML string.
    public static void sscEscape(StringBuffer sb, String s) {
        int len = s.length();
        for (int i = 0; i < len; i++)
            sscEscape(sb, (int) s.charAt(i));
    }

    // Convert output data string to escaped XML string.
    public static String sscEscape(String s) {
        StringBuffer sb = new StringBuffer();
        sscEscape(sb, s);
        return sb.toString();
    }

    /**
     * Reports given error as IllegalArgumentException, with location info. The
     * error must be related to some item of converted Monk input.
     * 
     * @param m
     *            any Monk data item, to carry origin
     * @param msg
     *            the error text string
     */
    private static void die(MonkData m, String msg)
            throws IllegalArgumentException {
        String file = m.loc.getFile();
        int line = m.loc.getLine();
        throw new IllegalArgumentException((file == null ? ""
                : ("file " + file + ": "))
                + (line == 0 ? "" : ("line " + line + ": ")) + msg);
    }
    
    /**
     * Warns on non-supported features
     * 
     * @param m any Monk data item
     * @param code the code of the non-supported feature
     * @param msg the warning message
     */
    private void noSupport(MonkData m, int code, String msg) {
        String file = m.loc.getFile();
        int line = m.loc.getLine();
        msg = (file == null ? ""
                : ("file " + file + ": "))
                + (line == 0 ? "" : ("line " + line + ": "))
                + (msg == null ? "" : msg);
        NoSupport noSupport = new NoSupport(code, msg);
        if (debug) {
            System.out.print("*** WARNING!!! ");
            System.out.println(noSupport);
        }
        notSupported.add(noSupport);
    }

    /**
     * Warns on non-supported features
     * 
     * @param m any Monk data item
     * @param code the code of the non-supported feature
     */
    private void noSupport(MonkData m, int code) {
        noSupport(m, code, null);
    }
    
    /**
     * Outputs non-negative N-digit number to string buffer.
     * 
     * @param sb
     *            buffer to emit string
     * @param num
     *            the number to emit
     * @param len
     *            number of digits
     * @param base
     *            numeric base, in range 2-16
     */
    private static void numOut(StringBuffer sb, int num, int len, int base) {
        char[] c = new char[len];
        for (; len-- > 0; num /= base)
            c[len] = "0123456789abcdef".charAt(num % base);
        sb.append(c);
    }

    /**
     * Tests if a character is a regex meta-character.
     * 
     * @param c the character
     * @return <code>true</code> if it is meta-character.
     */
    private static boolean isRegexMetachar(char c) {
        switch (c) {
        case '.':
        case '?':
        case '*':
        case '+':
        case '^':
        case '$':
        case '(':
        case ')':
        case '{':
        case '}':
        case '[':
        case ']':
            return true;
        }
        return false;
    }
    
    /**
     * Tests if a character can start a escape sequence.
     * 
     * @param c the character
     * @return <code>true</code> if the character is a valid character
     *          to start an escape sequence.
     */
    private static boolean isEscapeStart(char c) {
        switch (c) {
        case '0':
        case 'x':
        case 'u':
        case 't':
        case 'n':
        case 'r':
        case 'f':
        case 'a':
        case 'e':
        case 'c':
            return true;
        }
        return false;
    }
    
    /**
     * Converts a regex used in SSC to a regex that Java regex API can process.
     *   
     * @param regex the regular expression used in SSC
     * @return the converted regex that Java regex API can use.
     */
    private static String convRegex(String regex) {
        if (regex == null || regex.length() < 2) {
            return regex;
        }
        StringBuilder sb = new StringBuilder();
        final int PLANE = 0;
        final int ONE_BS = 1;
        final int TWO_BS = 2;
        final int THREE_BS = 3;
        int state = PLANE;
        final int size = regex.length();
        for (int i = 0; i < size; i++) {
            char c = regex.charAt(i);
            switch (state) {
            case THREE_BS:
                sb.append('\\').append(c);
                state = PLANE;
                continue;
            case TWO_BS:
                if (c == '\\') {
                    state = THREE_BS;
                } else {
                    sb.append(c);
                    state = PLANE;
                }
                continue;
            case ONE_BS:
                if (c == '\\') {
                    state = TWO_BS;
                } else {
                    if (isEscapeStart(c)) {
                        sb.append('\\').append(c);
                    } else {
                        sb.append(c);
                    }
                    state = PLANE;
                }
                continue;
            default:
                if (c == '\\') {
                    state = ONE_BS;
                } else if (isRegexMetachar(c)){
                    sb.append('\\').append(c);
                } else {
                    sb.append(c);
                }
            }
        }
        switch (state) {
        case THREE_BS:
            //illegal state.  Treat it as four backslashes
            sb.append("\\\\");
            break;
        case TWO_BS:
            sb.append('\\');
            break;
        case ONE_BS:
            //illegal state.  Treat it as two backslashes.
            sb.append('\\');
        }
        return sb.toString();
    }
    
    /**
     * This function translates a tree of SSC files to a tree of encoder
     * definition XSD files. The input tree will mirror the output tree, so that
     * each input SSC file corresponds to one output encoder definition XSD
     * file, and the relative path between two input files will be the same as
     * the relative path between the two corresponding output files.
     * The base-name of an input and its output file will remain the same,
     * but the suffix changes from ".ssc" to ".xsd".
     *
     * <p>
     * The input to the function is:
     *
     * (1) The root of the input tree.
     *     All further input file identification must be in/below this
     *     directory.
     *
     * (2) The root of the output tree.
     *     All generated output encoder definition XSD files will be in or
     *     below this directory.  The function will create any sub-directories
     *     if (and only if) needed.
     *
     * (3) The Monk load-path list.
     *     This is an (optionally empty) ordered list of directories, relative
     *     to the input root, that external template references in the SSC
     *     file could be relative to (rather like "-I" for a C compiler).
     *     If any of the files to be converted contains external references,
     *     then the reference *must* be resolvable along the load-path,
     *     because the reference paths are normalized in the output.
     *
     * (4) The SSC file list.
     *     The SSC file list is a (possibly empty) unordered list of input
     *     files, all specified as paths relative to the input tree root.
     *
     * (5) The target namespace
     *     The target namespace of the encoder definition files being created.
     * <p>
     * Besides the specified SSC files, the referenced external template files
     * will be converted as well. The output of the function, aside from logs
     * and errors, is:
     *
     * (1) The output tree of encoder definition XSD files.
     *
     * (2) A list of input to output file correspondences.
     *
     * (3) A list of external references, each reference consisting of the
     *     relative name of the input file (below the input tree root), the
     *     relative name of the referred input file, and the name of what
     *     *would* be the name of the corresponding output path for the
     *     referred file (i.e. with translated output directory names).
     * <p>
     * All encoder definition XSD files created will have same target namespace
     * and external template files are included using xsd:include
     * schema composition semantics by other encoder definition XSD files that
     * use them. 
     *
     * @param idir  input directory of SSC files
     * @param odir  output directory of XSC files
     * @param loads  Monk load-path, directories relative to idir
     * @param sscs  SSC files, relative to idir
     * @param targetNS target namespace of the encoder definitions
     * @param uconv  convert non-ASCII to U-escape in names?
     * @param sx  output: SSC to encoder mapping list (lines)
     * @param extern  output: list of loaded files not in sscs[]
     * @param notSupported features in SSC not supported by Encoder
     *
     * @throws IOException for I/O problems
     * @throws XmlException 
     */
    public static void convert(File idir, File odir, String[] loads,
            String[] sscs, String targetNS, boolean uconv, StringBuffer sx,
            Set<String> extern, Set<NoSupport> notSupported)
            throws IOException, XmlException {
        /*-
        if (debug)
            System.out.println("[ trans <" + idir.getPath()
            + "> to <" + odir.getPath() + ">, prepack=\""
            + prepack + "\" ]");
        -*/

        // Sanity checks.
        if (sscs.length == 0)
            return;
        if (idir == null)
            throw new IOException("convert: no input directory");
        if (!idir.exists())
            throw new IOException("convert: input directory \""
                    + idir.getPath() + "\" does not exist");
        if (odir == null)
            throw new IOException("convert: no output directory");
        if (!odir.exists())
            throw new IOException("convert: output directory \""
                    + odir.getPath() + "\" does not exist");

        Ssc2Custom sscConv = new Ssc2Custom(idir, loads, targetNS);
        HashSet<String> externThisBatch = new HashSet<String>();
        do {
            sscConv.trans(odir, sscs, sx, externThisBatch);
            if (notSupported != null) {
                notSupported.addAll(sscConv.notSupported);
            }
            extern.addAll(externThisBatch);
            sscs = externThisBatch.toArray(new String[0]);
            externThisBatch.clear();
        } while (sscs.length > 0);
    }

    /**
     * Translates the given list of SSC files, using the context provided
     * by the class instance for translation context.
     * Also adds a line for each load to "sx" for each load, with 3 space
     * separated fields: SSC file name, loaded file (resolved), and the
     * equivalent Encoder definition file.
     *
     * @param odir  output directory of encoder defintion XSD files
     * @param sscs  array of SSC files, relative to idir
     * @param sx  output: SSC to encoder definition mapping list (lines)
     * @param extern  output: set of loaded files not in sscs[]
     *
     * @throws IOException for I/O problem
     * @throws FileNotFoundException for SSC files in list but not on disk
     * @throws UnsupportedEncodingException for encoding problems in SSC
     * @throws XmlException XmlBeans binding related exception
     */
    public void trans(File odir, String[] sscs, StringBuffer sx,
            Set<String> extern) throws IOException, FileNotFoundException,
            UnsupportedEncodingException, XmlException {
        /* For each name sscs[i], generate the output filename encoders[i].
         * Make sure this does not generate duplicate pathnames because of
         * the translation of directories for Java package name compliance.
         */
        String[] encoders = new String[sscs.length];
        for (int i = 0; i < sscs.length; i++) {
            File ssc = new File(sscs[i]);
            String parent = ssc.getParent();
            String sscDir = ((parent == null) ? "" : (parent + "/").replace(
                    '\\', '/'));
            String xscDir = sscDir;
            String base = ssc.getName();
            if (base.endsWith(".ssc")) {
                base = base.substring(0, base.length() - 4);
            }
            encoders[i] = xscDir + base + ".xsd";
            sx.append("CONV " + sscs[i] + " TO " + encoders[i] + "\n");
        }

        // Loop over the original SSC files.
        for (int i = 0; i < sscs.length; i++) {
            // Generate Encoder text for SSC input.
            String parent = (new File(encoders[i])).getParent();
            if (parent != null)
                parent = parent.replace('\\', '/');
            convertOne(sscs[i], new File(odir, encoders[i]), targetNamespace,
                    sx, extern);
        }

        // Delete the original SSC files from the external set.
        for (int i = 0; i < sscs.length; i++)
            extern.remove(sscs[i]);
    }

    /**
     * Reads the given SSC file, converts it to the given Encoder output file.
     * Handles alternate encoding.
     * 
     * @param ssc
     *            SSC input file name (below input root)
     * @param encoder
     *            Encoder output file name (below output root)
     * @param targetNamespace
     *            target namespace of the encoder
     * @param sx
     *            output: SSC to Encoder mapping list (lines)
     * @param extern
     *            output: list of loaded files not in sscs[]
     * @throws XmlException 
     */
    public void convertOne(String ssc, File encoder, String targetNamespace,
            StringBuffer sx, Set<String> extern) throws IOException, XmlException {
        if (debug) {
            System.out.println("[ convertOne <" + ssc + "> to <"
                    + encoder.getPath() + ">, targetNamespace=<"
                    + targetNamespace + "> ]");
        }

        // Put Encoder file into output tree.
        File encoderDir = encoder.getParentFile();
        if (encoderDir != null && !encoderDir.isDirectory()) {
            encoderDir.mkdirs();
        }
        if (encoder.exists()) {
            encoder.delete();
        }
        
        // Create the encoder definition XML schema 
        SchemaDocument schemaDoc = SchemaDocument.Factory.newInstance();
        schemaDoc.addNewSchema();
        Schema schema = schemaDoc.getSchema();
        if (targetNamespace != null && targetNamespace.length() > 0) {
            schema.setTargetNamespace(targetNamespace);
            schema.setElementFormDefault(FormChoice.QUALIFIED);
        }
        EncodingMark encMark = EncodingMark.Factory.newInstance();
        encMark.setSource(ENCODING_NS_URI);
        Encoding encoding = encMark.addNewEncoding();
        encoding.setName(CUSTOM_ENCODING_NAME);
        encoding.setNamespace(CUSTOM_ENCODING_NS_URI);
        encoding.setStyle(CUSTOM_ENCODING_STYLE);
        CreatedBy createdBy = encMark.addNewCreatedBy();
        createdBy.setApplication("ETD Custom Encoder Converter");
        Version version = createdBy.addNewVersion();
        version.setMajor(0);
        version.setMinor(5);
        version.setRevision("0001");
        Annotation anno = schema.addNewAnnotation();
        anno.addNewAppinfo().set(encMark);
        NodeProperties rootNodeProp = NodeProperties.Factory.newInstance();
        
        sscEncoding = null;
        String etdName = null;
        DelimiterSet delimSet = null;
        
        // Iterate through file top-level elements.
        MonkLexer in = new MonkLexer((new File(sscInputRoot, ssc)).getPath());
        while (true) {
            MonkData m = in.next();
            if (m.type == MonkData.SC_EOF)
                break;
            if (!m.isPair())
                die(m, "unexpected " + m.typeName() + " at top-level");

            // Strip leading keyword.
            MonkPair mp = (MonkPair) m;
            if (!mp.car.isSymbol())
                die(m, "expected list to start with keyword, not "
                        + mp.car.typeName());
            String key = mp.car.getSymbol();

            // We have "(<key> ...)".
            if (key.equals("set-file-encoding-method")) {
                /*
                 * Change of encoding. Interpret this by switching the input
                 * lexer to a new charset encoding.
                 */
                if (cdes == null)
                    cdes = new MonkMatch("encoding clause", "-YY)", 1);
                cdes.match(true, m, null);
                sscEncoding = cdes.out(0).getSymbol();
                if (sscEncoding.startsWith(":"))
                    sscEncoding = sscEncoding.substring(1);
                in.setEncoding(sscEncoding);
            } else if (key.equals("load")) {
                /*
                 * External template declaration. Look it up here once. Note
                 * that a load does not necessarily mean the template will be
                 * used by references in the event structure. SSC files contains
                 * a "load" statement for each external template. Note that they
                 * follow any set-file-encoding-method calls in the SSC files
                 * generated by the Monk ETD editor, but we ignore the encoding
                 * for now...
                 */
                if (ldes == null) {
                    ldes = new MonkMatch("load statement", "-YS)", 1);
                }
                ldes.match(true, m, null);
                String rawName = ldes.out(0).getString();
                if (rawName == null) {
                    die(m, "Invalid load-stmt.");
                }
                String relName = normalizeLoad(rawName);
                if (debug) {
                    System.out.println("[ - search <" + rawName + "> ]");
                }
                if (relName == null) {
                    throw new RuntimeException("cannot resolve \"" + rawName
                            + "\" reference in \"" + ssc + "\"");
                }
                /*
                 * Found file, now map directory. Note that the call to
                 * xscPathUnique() has the required side-effect of assuring the
                 * file-name mapping is unambiguous.
                 */
                if (debug) {
                    System.out.println("[ - found as <" + relName + "> ]");
                }
                File relFile = new File(relName);
                String encoderName = slashPath(relFile);
                extern.add(encoderName);
                if (encoderName.endsWith(".ssc")) {
                    encoderName =
                        encoderName.substring(0, encoderName.length() - 4);
                    encoderName = encoderName + ".xsd";
                }
                if (debug) {
                    System.out.println("[ - output as <" + relName + "> ]");
                }
                sx.append("REPLACE " + ssc + " --loads--> " + slashPath(relFile)
                        + " WITH " + encoder.getName() + " --includes--> "
                        + encoderName + "\n");
                Include inc = schema.addNewInclude();
                inc.setSchemaLocation(encoderName);
            } else if (key.equals("define")) {
                // Delimiter list, or template, else unknown.
                if (debug) {
                    System.out.println("[ seen define ]");
                }
                if (ddes == null) {
                    ddes = new MonkMatch("define clause", "-YYX)", 2);
                }
                ddes.match(true, m, null);
                MonkData sym = ddes.out(0);
                MonkData def = ddes.out(1);
                if (def.isPair() && def.getCar().isSymbol("quote")) {
                    /*
                     * Got a "(define <name>-delm (quote ...))" delimiter list.
                     * If the SSC file contains non-standard code using a define
                     * with a quoted argument, we will barf.
                     */
                    if (debug) {
                        System.out.println("[ seen delimiter list ]");
                    }
                    
                    delimSet = rootNodeProp.addNewDelimiterSet();
                    for (MonkData d = def.getCdr().getCar(); d.isPair(); d = d
                            .getCdr()) {
                        convDelim(d.getCar(), delimSet, rootNodeProp);
                    }
                    if (delimSet.sizeOfLevelArray() == 0) {
                        delimSet = null;
                        rootNodeProp.unsetDelimiterSet();
                    }
                } else if (sym.isSymbol()
                        && def.isPair()
                        && (def.getCar().isSymbol("message-convert") || def
                                .getCar().isSymbol("$resolve-event-definition"))
                        && def.getCdr().getCar().isPair()
                        && def.getCdr().getCar().getCar().isSymbol("quote")) {
                    /*
                     * Here we have the start of a field list, with this syntax:
                     * (define NAME-struct (message-convert (quote FIELDS)))
                     * This must be either a local template definition or the
                     * top-level structure; they look the same. Note:
                     * "$resolve-event-definition" is the newer form.
                     */
                    if (debug) {
                        System.out.println("[ seen message struct ]");
                    }

                    // Check name, strip "-struct" suffix.
                    etdName = sym.getSymbol();
                    if (etdName.endsWith("-struct"))
                        etdName = etdName.substring(0, etdName.length() - 7);
                    else {
                        /*-
                        //Name does not end with "-struct" !?
                        warn("warning: non-struct message name <"
                            + name + ">\n")
                        -*/
                    }
                    convField(schema, null,
                            def.getCdr().getCar().getCdr().getCar(), 0,
                            rootNodeProp);
                }
            } else {
                // Unexpected at top. Maybe this should warn and skip...
                // Skip it, might be some Monk function call
                //die(m, "unknown keyword \"" + key + "\"");
            }
        }
        in.close();
        if (etdName == null) {
            throw new IOException("file \"" + ssc
                    + "\" does not contain a main node");
        }
        TopLevelElement[] topElems = schema.getElementArray();
        for (int i = 0; i < topElems.length; i++) {
            if (etdName.equals(topElems[i].getName())) {
                XmlObject xmlObj =
                    topElems[i].getAnnotation().getAppinfoArray(0);
                CustomEncoding customEncoding;
                if (xmlObj instanceof CustomEncoding) {
                    customEncoding = (CustomEncoding) xmlObj;
                } else {
                    customEncoding = CustomEncoding.Factory.parse(
                            xmlObj.xmlText());
                }
                // main node is always assumed to be a group node
                NodeProperties nodeProp = customEncoding.getNodeProperties();
                
                //<==Set top level properties
                customEncoding.setTop(true);
                nodeProp.setFineInherit(true);
                //merge rootNodeProp to this nodeProp
                if (rootNodeProp.isSetUndefDataPolicy()) {
                    nodeProp.setUndefDataPolicy(
                            rootNodeProp.getUndefDataPolicy());
                }
                if (rootNodeProp.isSetEscapeSequence()) {
                    nodeProp.setEscapeSequence(
                            rootNodeProp.getEscapeSequence());
                }
                //==>
                
                if (delimSet != null) {
                    //if the root node is delimiter or array, add a dummy level
                    //at the beginning to delimSet.  This is because that in
                    //ETD top delim node always does not consume delimiter.
                    if (NodeType.DELIMITED.equals(nodeProp.getNodeType())
                            || NodeType.ARRAY.equals(nodeProp.getNodeType())) { 
                        DelimiterLevel dummyLevel = delimSet.insertNewLevel(0);
                        Delimiter dummyDelim = dummyLevel.addNewDelimiter();
                        dummyDelim.setOrigin(Origin.DUMMY);
                        dummyDelim.addNewBytes().setConstant("\\x00");
                        dummyDelim.setOptionalMode(OptionalMode.NEVER);
                        dummyDelim.setTerminatorMode(TerminatorMode.NEVER);
                    }                    
                    if (!nodeProp.isSetDelimiterSet()) {
                        nodeProp.setDelimiterSet(delimSet);
                    } else {
                        //Need to merge two delimiter sets
                        DelimiterSet localDelimSet = nodeProp.getDelimiterSet();
                        for (int k = 0; k < localDelimSet.sizeOfLevelArray();
                                k++) {
                            if (k < delimSet.sizeOfLevelArray()) {
                                delimSet.setLevelArray(k,
                                        localDelimSet.getLevelArray(k));
                            } else {
                                delimSet.addNewLevel();
                                // Since k is incremented sequentially, adding
                                // one new level should be enough.
                                delimSet.setLevelArray(k,
                                        localDelimSet.getLevelArray(k));
                            }
                        }
                    }
                }
                topElems[i].getAnnotation().getAppinfoArray(0).set(
                        customEncoding);
            }
        }
        schemaDoc.save(encoder);
    }

    /**
     * Converts given global delimiter specification. Outputs result to "e".
     * 
     * @param d delimiter specification
     * @param delimSet delimiter set that needs to be populated
     * @param rootNodeProp root node properties
     */
    private void convDelim(MonkData d, DelimiterSet delimSet,
            NodeProperties rootNodeProp) {
        
        DelimiterLevel delimLevel = delimSet.addNewLevel();
        Delimiter delim;
        boolean isArray = false;
        OptionalMode.Enum optionalMode = OptionalMode.ALLOW;
        TerminatorMode.Enum termMode = TerminatorMode.ALLOW;
        boolean beginAnch = false;
        boolean endAnch = false;
        
        for (; d.isPair(); d = d.getCdr()) {
            MonkData del = d.getCar();
            if (del.isSymbol()) {
                // The delimiter flags.
                String as = del.getSymbol();
                if (as.equals("anchored")) {
                    beginAnch = true;
                    endAnch = true;
                } else if (as.equals("beginanchored")) {
                    beginAnch = true;
                } else if (as.equals("endanchored")) {
                    endAnch = true;
                } else if (as.equals("array")) {
                    isArray = true;
                } else if (as.equals("endofrec")) {
                    termMode = TerminatorMode.FAVOR;
                } else if (as.equals("required")) {
                    termMode = TerminatorMode.FORCE;
                } else if (as.equals("separator")) {
                    termMode = TerminatorMode.NEVER;
                } else {
                    die(del, "unconvertible modifier [" + as + "]");
                }
            } else if (del.isString()) {
                // Old-style simple delimiter: single end-delimiter.
                if (((MonkString) del).length < 1) {
                    die(del, "delimiter can't be empty string");
                }
                delim = delimLevel.addNewDelimiter();
                delim.setOrigin(Origin.DEFAULT);
                makeEndDelim(del, delim);
            } else if (del.isInteger()) {
                // Offset/length encoded delimiter.
                long offset = ((MonkInteger) del).value;
                long length = 1;
                if (d.getCdr() != null && d.getCdr().isPair()
                        && d.getCdr().getCar().isInteger()) {
                    // Consume 2nd argument: length.
                    length = ((MonkInteger) d.getCdr().getCar()).value;
                    d = d.getCdr();
                }
                if (offset < 0 || offset > Integer.MAX_VALUE)
                    die(del, "delimiter offset value negative "
                            + "or too large");
                if (length < 1 || length > Short.MAX_VALUE)
                    die(del, "delimiter length value negative, "
                            + "zero or too large");
                delim = delimLevel.addNewDelimiter();
                delim.setOrigin(Origin.DEFAULT);
                DelimiterBytes bytes = delim.addNewBytes();
                EmbeddedBytes embedded = bytes.addNewEmbedded();
                embedded.setOffset((int) offset);
                embedded.setLength((short) length);
            } else if (del.isPair()) {
                // Sublist.
                MonkData part = del.getCar();
                if (part == null)
                    die(del, "empty sublist in delimiter");
                if (part.isString()) {
                    // Simple begin/end delimiter pair
                    MonkData bd = part;
                    MonkData ed = del.getCdr().getCar();
                    if (bd != null && bd.isString() && ed != null
                            && ed.isString()) {
                        if (((MonkString) bd).length < 1
                                || ((MonkString) ed).length < 1) {
                            die(del, "delimiter can't be empty string");
                        }
                        delim = delimLevel.addNewDelimiter();
                        delim.setOrigin(Origin.DEFAULT);
                        makeBeginDelim(bd, delim);
                        makeEndDelim(ed, delim);
                    } else {
                        die(del, "malformed delimiter pair; "
                                + "expected two strings, got "
                                + (bd == null ? "nothing" : bd.typeName())
                                + " and "
                                + (ed == null ? "nothing" : ed.typeName()));
                    }
                } else if (part.isSymbol()) {
                    if (part.isSymbol("EscD")) {
                        // escape delimiter list.
                        for (del = del.getCdr(); del.isPair(); del = del
                                .getCdr()) {
                            part = del.getCar();
                            if (part.isString()) {
                                if (((MonkString) part).length < 1) {
                                    //Empty Esc string should considered
                                    //as not set
                                    //die(part,
                                    //        "escape delimiter can't be empty string");
                                    continue;
                                }
                                if (rootNodeProp.isSetEscapeSequence()) {
                                    //TODO: Multiple escape sequences are
                                    //not supported.
                                    noSupport(part,
                                            NoSupport.MULTIPLE_ESC_SEQUENCES);
                                } else {
                                    rootNodeProp.setEscapeSequence(
                                            Misc.escapeString(
                                                    new String(((MonkString) part).value)));
                                }
                            } else {
                                die(del, "escape delim sublist part can't be "
                                        + part.typeName());
                            }
                        }
                    } else {
                        boolean begin = true;
                        delim = null;
                        // Mutiple/paired delimiter list.
                        for (; del.isPair(); del = del.getCdr()) {
                            part = del.getCar();
                            if (part.isSymbol("Bd")) {
                                // Start of begin-delimiters. */
                                begin = true;
                            } else if (part.isSymbol("Ed")) {
                                // Start of end-delimiters. */
                                begin = false;
                            } else if (part.isString()) {
                                if (((MonkString) part).length < 1)
                                    die(part, "delimiter can't be empty string");
                                if (delim == null) {
                                    delim = delimLevel.addNewDelimiter();
                                    delim.setOrigin(Origin.DEFAULT);
                                }
                                if (begin) {
                                    makeBeginDelim(part, delim);
                                } else {
                                    makeEndDelim(part, delim);
                                    //Clear delim to make sure begin and end
                                    //delimiters are in pair.
                                    delim = null;
                                }
                            } else {
                                die(del, "sublist part can't be "
                                        + part.typeName());
                            }
                        }
                    }
                } else {
                    die(del, "sublist start can't be " + part.typeName());
                }
            } else {
                // Huh?
                die(del, "modifier can't be " + del.typeName());
            }
        }
        if (isArray) {
            //remove the current level since the array delimiter belongs
            //to the previous level.
            int previousLevelIdx = delimSet.sizeOfLevelArray() - 2;
            DelimiterLevel previousLevel;
            int delimCount;
            if (previousLevelIdx >= 0
                    && ((previousLevel = delimSet.getLevelArray(previousLevelIdx)) != null)
                    && (delimCount = delimLevel.sizeOfDelimiterArray()) > 0) {
                //Copy over all array delimiters to previous level.
                int countInLastLevel = previousLevel.sizeOfDelimiterArray();
                for (int i = delimCount - 1; i >= 0; i--) {
                    Delimiter tmpDelim = delimLevel.getDelimiterArray(i);
                    tmpDelim.setKind(Kind.REPEAT);
                    if (!tmpDelim.isSetOptionalMode()) {
                        tmpDelim.setOptionalMode(optionalMode);
                    }
                    if (!tmpDelim.isSetTerminatorMode()) {
                        tmpDelim.setTerminatorMode(termMode);
                    }
                    if (!beginAnch) {
                        tmpDelim.setBeginAnch(false);
                    }
                    if (!endAnch) {
                        tmpDelim.setEndAnch(false);
                    }
                    previousLevel.addNewDelimiter();
                    previousLevel.setDelimiterArray(countInLastLevel, tmpDelim);
                    countInLastLevel++;
                    delimLevel.removeDelimiter(i);
                }
                delimSet.removeLevel(delimSet.sizeOfLevelArray() - 1);
            } else {
                die(d, "Array delimiter must refer to a previous"
                        + " delimiter level.");
            }
        } else {
            for (int i = 0; i < delimLevel.getDelimiterArray().length; i++) {
                delim = delimLevel.getDelimiterArray(i);
                if (!delim.isSetKind()) {
                    //Set delimiter kind to normal for all delimiters except
                    //escape delimiters.
                    delim.setKind(Kind.NORMAL);
                }
                if (!delim.isSetOptionalMode()) {
                    delim.setOptionalMode(optionalMode);
                }
                if (!delim.isSetTerminatorMode()) {
                    delim.setTerminatorMode(termMode);
                }
                if (!beginAnch) {
                    delim.setBeginAnch(false);
                }
                if (!endAnch) {
                    delim.setEndAnch(false);
                }
            }
        }
    }

    /**
     * Given a list that describes a field and its subfields in an SSC file,
     * emits an encoder description for the field, and recursively for all of
     * its subfields. The normal syntax for a (non-template) field list is:
     * 
     * (name type lo hi in out offset length [subfield...])
     * 
     * with the following meaning:
     * 
     * type (ON,OF,OS,AS,...) -- kind of node: delimited, fixed, set... lo
     * (integer) -- lower bound on repetition hi (integer or INF) -- upper bound
     * on repetition (INF=no limit) in (string or und) -- input pattern (und=no
     * match) out (string or und) -- output default data (und = none) offset
     * (integer or und) -- offset (und=after previous) length (integer) -- total
     * length (0=sum of subs, -1=to end)
     * 
     * Preceding the name field may be a sublist of properties.
     * 
     * Template fields have a slightly different syntax : (name type lo hi file
     * struct-name und und) where "file" is the name of a file containing the
     * template.
     * 
     * Note: we make the hard assumption that the input has been checked to be a
     * proper list (cf. ssc-proper-list), to minimize input format checking. The
     * "used" and "opas" parameters must be a non-empty lists of names *not* to
     * use; this function will add the generated name to the list. If "top?" is
     * set, the field must not be a leaf node; add fake parent if required to
     * enforce this.
     * 
     * @param schema the schema object. Must not be <code>null</code>
     * @param parentElem the parent element declaration. May be <code>null</code>
     * @param fdes field to translate
     * @param depth nesting below template root (0=top-level)
     * @param rootNodeProp root node properties
     * @return whether a &lt;node&gt; entity was emitted
     * @throws IllegalArgumentException
     *             on invalid or unsupported SSC input
     */
    public boolean convField(Schema schema, Element parentElem, MonkData fdes,
            int depth, NodeProperties rootNodeProp)
    throws IllegalArgumentException, UnsupportedEncodingException {
        
        boolean top = (depth == 0);

        // Get list elements.
        if (mdes == null) {
            mdes = new MonkMatch("field description", "?LYYXXXXXX*", 10);
        }
        try {
            mdes.match(true, fdes, null);
        } catch (IllegalArgumentException ex) {
            // Create a more specific message.
            ex.printStackTrace();

            // Generate corresponding error message.
            StringBuffer buf = new StringBuffer("List too short.  Missing");
            switch (mdes.getLastIndex()) {
            case -1:
                buf = new StringBuffer("Empty list encountered.");
                break;
            case 1:
                buf.append(" all after node Name");
                break;
            case 2:
                buf.append(" all after node Type");
                break;
            case 3:
                buf.append(" all after node MinRep");
                break;
            case 4:
                buf.append(" all after node MaxRep");
                break;
            case 5:
                buf.append(" all after node InputMatch");
                break;
            case 6:
                buf.append(" all after node DefaultData");
                break;
            case 7:
                buf.append(" all after node ByteOffset");
                break;
            case 8:
                buf.append(" all after node Length");
                break;
            case 9:
                buf.append(" rest of node");
                break;

            // Unknown cause, re-throw exception.
            default:
                throw ex;
            }

            // Throw exception with more specific message.
            buf.append(" (" + ex.getMessage() + ")");
            throw new IllegalArgumentException(buf.toString());
        }

        MonkData attr = mdes.out(0);
        MonkSymbol name = (MonkSymbol) mdes.out(1);
        MonkSymbol type = (MonkSymbol) mdes.out(2);
        MonkData rmin = mdes.out(3);
        MonkData rmax = mdes.out(4);
        MonkData itag = mdes.out(5);
        MonkData otag = mdes.out(6);
        MonkData offs = mdes.out(7);
        MonkData tlen = mdes.out(8);
        MonkData rest = mdes.out(9);

        // Optional properties.
        boolean exac = false;
        boolean grpd = false;
        boolean ntag = false;
        boolean prec = false;
        boolean sout = false;
        MonkString bdel = null;
        MonkString edel = null;
        MonkString scav = null;
        MonkData cmin = null;
        MonkData cmax = null;
        MonkData precData = null;

        // Strip "-struct" stuff.
        if (top && name.value.endsWith("-struct")) {
            name = new MonkSymbol(name.loc, name.value.substring(0, name.value
                    .length() - 7));
        }
        Element newElemDecl;
        CustomEncoding customEncoding = CustomEncoding.Factory.newInstance();
        customEncoding.setSource(ENCODING_NS_URI);
        NodeProperties nodeProp = customEncoding.addNewNodeProperties();
        if (top) {
            newElemDecl = schema.addNewElement();
            nodeProp.setNodeType(NodeType.GROUP);
        } else {
            newElemDecl = getGroup(parentElem).addNewElement();
        }
        DelimiterSet delimSet = nodeProp.addNewDelimiterSet();
        
        // Infinity = -1.
        if (rmax.isSymbol("INF")) {
            rmax = new MonkInteger(rmax.loc, true, -1);
        }
        if (!rmax.isInteger()) {
            die(rmax, "non-integer maxOccurs value");
        }

        // Suppress info-only nodes...
        if (rmax.isInteger(0)) {
            return false;
        }

        // Check bounds to make sure they fit in an int.
        if (((MonkInteger) rmin).value > Integer.MAX_VALUE) {
            die(rmin, "min-rep value too large");
        }
        if (((MonkInteger) rmax).value > Integer.MAX_VALUE) {
            die(rmax, "max-rep value too large");
        }
        if (offs.isInteger()
                && ((MonkInteger) offs).value > Integer.MAX_VALUE) {
            die(offs, "offset value too large");
        }

        // Fake child to hold data, if top is leaf.
        if (top && (rest == null || rest.isNil())) {
            // (name type 1 1 und und und 0)
            MonkLoc loc = fdes.loc;
            MonkNil nil = new MonkNil(loc);
            MonkInteger one = new MonkInteger(loc, true, 1);
            MonkInteger zero = new MonkInteger(loc, true, 0);
            MonkSymbol und = new MonkSymbol(loc, "und");
            rest = new MonkPair(loc, name, new MonkPair(loc, type,
                    new MonkPair(loc, one, new MonkPair(loc, one, new MonkPair(
                            loc, und, new MonkPair(loc, und, new MonkPair(loc,
                                    und, new MonkPair(loc, zero, nil))))))));
            rest = new MonkPair(loc, rest, nil);
        }
        
        // Process the optional field attributes.
        boolean isParent = rest.isPair();
        DelimiterLevel delimLevel = delimSet.addNewLevel();
        Delimiter delim;
        OptionalMode.Enum optionalMode = OptionalMode.ALLOW;
        TerminatorMode.Enum termMode = TerminatorMode.ALLOW;
        List<Delimiter> normalDelims = new ArrayList<Delimiter>();
        boolean beginAnch = false;
        boolean endAnch = false;
        
        for (; attr != null && attr.isPair(); attr = attr.getCdr()) {
            MonkData a = ((MonkPair) attr).car;
            if (a.isSymbol()) {
                // Simple property.
                String sym = a.getSymbol();
                if (sym.equals("anchored")) {
                    beginAnch = true;
                    endAnch = true;
                } else if (sym.equals("beginanchored")) {
                    beginAnch = true;
                } else if (sym.equals("endanchored")) {
                    endAnch = true;
                } else if (sym.equals("endofrec")) {
                    termMode = TerminatorMode.FAVOR;
                } else if (sym.equals("required")) {
                    termMode = TerminatorMode.FORCE;
                } else if (sym.equals("separator")) {
                    termMode = TerminatorMode.NEVER;
                } else if (sym.equals("Bd")) {
                    // Monk ETD editor bug; ignore lone "Bd"
                } else if (sym.equals("Ed")) {
                    // Monk ETD editor bug; ignore lone "Ed"
                } else if (sym.equals("Ex")) {
                    exac = true;
                } else if (sym.equals("Gr")) {
                    grpd = true;
                } else if (sym.equals("Nt")) {
                    ntag = true;
                } else if (sym.equals("Pp")) {
                    precData = a;
                    prec = true;
                } else {
                    die(a, "unconvertible modifier [" + sym + "]");
                }
            } else if (a.isPair()) {
                // Property with arguments.
                MonkPair a2 = (MonkPair) a;
                if (a2.car.isSymbol()) {
                    String sym = a2.car.getSymbol();
                    MonkData arg1 = null;
                    if (a2.cdr.isPair()) {
                        arg1 = a2.cdr.getCar();
                    }
                    if (sym.equals("Bd") || sym.equals("Ed")) {
                        /*
                         * Modifier "Bd" (begin delimiter) is followed by one or
                         * more delimiters, and optionally by "Ed" (end
                         * delimiter) followed by some delimiters. If "Ed" is
                         * present, the Bd/Ed values form a separate delimiter
                         * group.
                         */
                        delim = null;
                        boolean begin = sym.equals("Bd");
                        for (MonkData a3 = a2.cdr; a3 != null;
                                a3 = a3.getCdr()) {
                            if (a3.isNil()) {
                                break;
                            }
                            if (!a3.isPair())
                                die(a, "malformed Bd modifier, tail is "
                                        + a3.typeName());
                            MonkData a4 = ((MonkPair) a3).car;
                            if (a4.isSymbol("Ed")) {
                                // Paired delimiter mark.
                                begin = false;
                            } else if (a4.isString()) {
                                // Explicit delimiter: string value.
                                MonkString dval = (MonkString) a4;
                                if (dval.length < 1) {
                                    die(a4, "delimiter cannot be empty string");
                                }
                                if (delim == null) {
                                    delim = delimLevel.addNewDelimiter();
                                }
                                if (begin) {
                                    makeBeginDelim(a4, delim);
                                } else {
                                    makeEndDelim(a4, delim);
                                    normalDelims.add(delim);
                                }
                            } else if (a4.isPair()) {
                                // Implicit delimiter: offset[,length] list.
                                die(a4, "local encoded delimiters not "
                                        + "supported (yet)");
                            } else {
                                // Huh?
                                die(a4, "unexpected " + a4.typeName()
                                        + " in Bd modifier");
                            }
                        }
                    } else if (sym.equals("EscD")) {
                        /*
                         * Modifier "EscD" (escape delimiter) is followed by one
                         * or more escape delimiters.
                         */
                        for (MonkData a3 = a2.cdr; a3 != null;
                                a3 = a3.getCdr()) {
                            if (a3.isNil()) {
                                break;
                            }
                            if (!a3.isPair())
                                die(a, "malformed EscD modifier, tail is "
                                        + a3.typeName());
                            MonkData a4 = ((MonkPair) a3).car;
                            if (a4.isString()) {
                                MonkString dval = (MonkString) a4;
                                if (dval.length < 1)
                                    die(a4,
                                            "escape delimiter cannot be empty string");
                                if (rootNodeProp.isSetEscapeSequence()) {
                                    //TODO: Multiple escape sequences are
                                    //not supported.
                                    noSupport(a4,
                                            NoSupport.MULTIPLE_ESC_SEQUENCES);
                                } else {
                                    rootNodeProp.setEscapeSequence(
                                            Misc.escapeString(new String(dval.value)));
                                }
                            } else {
                                // Huh?
                                die(a4, "unexpected " + a4.typeName()
                                        + " in EscD modifier");
                            }
                        }
                    } else if (sym.equals("Ri")) {
                        /*
                         * The "Ri" modifier should only occur on array nodes
                         * (ONA/ANA) and defines a local array delimiter.
                         */
                        MonkData arrAttr = a2.cdr;
                        if (arrAttr.isNil()) {
                            continue;
                        }
                        //Read array delimiters
                        boolean arrBeginAnch = false;
                        boolean arrEndAnch = false;
                        OptionalMode.Enum arrOptionalMode = OptionalMode.ALLOW;
                        TerminatorMode.Enum arrTermMode = TerminatorMode.ALLOW;
                        List<Delimiter> delims = new ArrayList<Delimiter>();
                        for (; arrAttr != null && arrAttr.isPair();
                                arrAttr = arrAttr.getCdr()) {
                            MonkData arrA = ((MonkPair) arrAttr).car;
                            if (arrA.isSymbol()) {
                                // Simple property.
                                String arrSym = arrA.getSymbol();
                                if (arrSym.equals("anchored")) {
                                    arrBeginAnch = true;
                                    arrEndAnch = true;
                                } else if (arrSym.equals("beginanchored")) {
                                    arrBeginAnch = true;
                                } else if (arrSym.equals("endanchored")) {
                                    arrEndAnch = true;
                                } else if (arrSym.equals("endofrec")) {
                                    arrTermMode = TerminatorMode.FAVOR;
                                } else if (arrSym.equals("required")) {
                                    arrOptionalMode = OptionalMode.FORCE;
                                } else if (arrSym.equals("separator")) {
                                    arrTermMode = TerminatorMode.NEVER;
                                } else {
                                    die(arrA, "unconvertible modifier ["
                                            + arrSym + "]");
                                }
                            } else if (arrA.isPair()) {
                                // Property with arguments.
                                MonkPair arrA2 = (MonkPair) arrA;
                                if (arrA2.car.isSymbol()) {
                                    String arrSym = arrA2.car.getSymbol();
                                    MonkData arrArg1 = null;
                                    if (arrA2.cdr.isPair()) {
                                        arrArg1 = arrA2.cdr.getCar();
                                    }
                                    if (arrSym.equals("Bd")
                                            || arrSym.equals("Ed")) {
                                        /*
                                         * Modifier "Bd" (begin delimiter) is
                                         * followed by one or more delimiters,
                                         * and optionally by "Ed" (end
                                         * delimiter) followed by some
                                         * delimiters. If "Ed" is present,
                                         * the Bd/Ed values form a separate
                                         * delimiter group.
                                         */
                                        boolean begin = arrSym.equals("Bd");
                                        delim = null;
                                        for (MonkData a3 = arrA2.cdr;
                                                a3 != null; a3 = a3.getCdr()) {
                                            if (a3.isNil()) {
                                                break;
                                            }
                                            if (!a3.isPair()) {
                                                die(arrA, "malformed Bd "
                                                        + "modifier, tail is "
                                                        + a3.typeName());
                                            }
                                            MonkData a4 = ((MonkPair) a3).car;
                                            if (a4.isSymbol("Ed")) {
                                                // Paired delimiter mark.
                                                begin = false;
                                            } else if (a4.isString()) {
                                                // Explicit delimiter:
                                                // string value.
                                                MonkString dval =
                                                    (MonkString) a4;
                                                if (dval.length < 1) {
                                                    die(a4, "delimiter cannot"
                                                            + "be empty string");
                                                }
                                                if (delim == null) {
                                                    delim = delimLevel.addNewDelimiter();
                                                    delim.setKind(Kind.REPEAT);
                                                }
                                                if (begin) {
                                                    makeBeginDelim(a4, delim);
                                                } else {
                                                    makeEndDelim(a4, delim);
                                                    delims.add(delim);
                                                }
                                            } else if (a4.isPair()) {
                                                long offset = ((MonkInteger) ((MonkPair) a4).car).value;
                                                long length = 1;
                                                if (a4.getCdr() != null && a4.getCdr().isPair()
                                                        && a4.getCdr().getCar().isInteger()) {
                                                    // Consume 2nd argument: length.
                                                    length = ((MonkInteger) a4.getCdr().getCar()).value;
                                                }
                                                if (offset < 0 || offset > Integer.MAX_VALUE)
                                                    die(a4, "delimiter offset value negative "
                                                            + "or too large");
                                                if (length < 1 || length > Short.MAX_VALUE)
                                                    die(a4, "delimiter length value negative, "
                                                            + "zero or too large");
                                                Delimiter eDelim = delimLevel.addNewDelimiter();
                                                DelimiterBytes bytes = eDelim.addNewBytes();
                                                EmbeddedBytes embedded = bytes.addNewEmbedded();
                                                embedded.setOffset((int) offset);
                                                embedded.setLength((short) length);
                                                eDelim.setKind(Kind.REPEAT);
                                                delims.add(eDelim);
                                            } else {
                                                // Huh?
                                                die(a4, "unexpected " + a4.typeName()
                                                        + " in Bd modifier");
                                            }
                                        }
                                    } else {
                                        // We don't (yet) support e.g. "BdB".
                                        die(arrA2, "unconvertible modifier [" + sym + "]");
                                    }
                                } else if (arrA2.car.isString()) {
                                    /*
                                     * This must be an abbreviated local
                                     * delimiter pair, e.g.:
                                     * ((("[" "]") endofrec) name ON ...)
                                     */
                                    bdel = null;
                                    edel = (MonkString) arrA2.car;
                                    if (arrA2.cdr.isPair()
                                            && arrA2.cdr.getCar().isString()) {
                                        bdel = edel;
                                        edel = (MonkString) arrA2.cdr.getCar();
                                    }
                                    Delimiter rDelim = delimLevel.addNewDelimiter();
                                    if (bdel != null) {
                                        makeBeginDelim(bdel, rDelim);
                                    }
                                    makeEndDelim(edel, rDelim);
                                    rDelim.setKind(Kind.REPEAT);
                                    delims.add(rDelim);
                                } else {
                                    // Impossible.
                                    die(a, "modifier can't start with " + arrA2.typeName());
                                }
                            } else {
                                die(a, "modifier must be <symbol>, not <" + arrA.typeName() + ">");
                            }
                        }
                        for (Delimiter d : delims) {
                            d.setOptionalMode(arrOptionalMode);
                            d.setTerminatorMode(arrTermMode);
                            if (!arrBeginAnch) {
                                d.setBeginAnch(false);
                            }
                            if (!arrEndAnch) {
                                d.setEndAnch(false);
                            }
                        }
                    } else if (sym.equals("Sc") || sym.equals("ScN")) {
                        if (arg1 == null || !arg1.isString()) {
                            die(a2, "[Sc/ScN] scavenger modifier requires "
                                    + "string argument");
                        }
                        scav = (MonkString) arg1;
                        sout = sym.equals("Sc");
                    } else if (sym.equals("NofN")) {
                        /*
                         * The "NofN" has five valid forms: (NofN lower) --
                         * upper bound is infinity (NofN lower upper) --
                         * deprecated form (NofN lower "INF") -- deprecated form
                         * (NofN (lower upper)) -- the normal case (NofN (lower
                         * "INF")) -- allowable explicit infinity
                         */
                        if (arg1 == null)
                            die(a2, "[NofN] modifier requires argument");
                        if (arg1.isInteger()) {
                            cmin = arg1;
                            cmax = a2.cdr.getCdr().getCar();
                            if (cmax == null)
                                cmax = new MonkSymbol(cmin.loc, "INF");
                        } else if (arg1.isPair()) {
                            cmin = arg1.getCar();
                            cmax = arg1.getCdr().getCar();
                            if (cmin == null || cmax == null
                                    || !cmin.isInteger())
                                die(a2, "[NofN] invalid argument list");
                        } else {
                            // First argument is not an allowable form.
                            die(a2, "[NofN] requires integer or list, not "
                                    + arg1.typeName());
                        }
                        if (cmin == null || !cmin.isInteger()) {
                            die(arg1,
                                    "[NofN] lower-bound must be integer, "
                                            + "not "
                                            + (cmin == null ? "empty" : cmin
                                                    .typeName()));
                        }

                        if (((MonkInteger) cmin).value > Integer.MAX_VALUE) {
                            die(cmin, "child-min value too large");
                        }

                        if (cmax.isInteger()
                                && ((MonkInteger) cmax).value
                                    > Integer.MAX_VALUE) {
                            die(cmax, "child-max value too large");
                        }
                    } else {
                        // We don't (yet) support e.g. "BdB".
                        die(a2, "unconvertible modifier [" + sym + "]");
                    }
                } else if (a2.car.isString()) {
                    /*
                     * This must be an abbreviated local delimiter pair, e.g.:
                     * ((("[" "]") endofrec) name ON ...)
                     */
                    bdel = null;
                    edel = (MonkString) a2.car;
                    if (a2.cdr.isPair() && a2.cdr.getCar().isString()) {
                        bdel = edel;
                        edel = (MonkString) a2.cdr.getCar();
                    }
                    delim = delimLevel.addNewDelimiter();
                    if (bdel != null) {
                        makeBeginDelim(bdel, delim);
                    }
                    makeEndDelim(edel, delim);
                    normalDelims.add(delim);
                } else {
                    // Impossible.
                    die(a, "modifier can't start with " + a2.typeName());
                }
            } else {
                // We don't (yet) support e.g. "Ri".
                die(a, "modifier must be <symbol>, not <" + a.typeName() + ">");
            }
        }

        String tp = type.value;
        if (tp.equals("ON") || tp.equals("OF") || tp.equals("OS")
                || tp.equals("ONA") || tp.equals("AN") || tp.equals("AF")
                || tp.equals("AS") || tp.equals("ANA")) {
            // Normal fields.
            newElemDecl.setName(name.value);
            
            String order, struc;
            switch (tp.charAt(0)) {
            case 'O':
                if (isParent) {
                    nodeProp.setOrder(Order.SEQUENCE);
                }
                order = "sequence";
                break;
            case 'A':
                if (isParent) {
                    nodeProp.setOrder(Order.MIXED);
                }
                order = "any";
                break;
            default:
                order = "?";
            }
            switch (tp.charAt(1)) {
            case 'F':
                nodeProp.setNodeType(NodeType.FIXED_LENGTH);
                struc = "fixed";
                break;
            case 'S':
                nodeProp.setNodeType(NodeType.GROUP);
                struc = "set";
                break;
            case 'N':
                if (tp.length() == 2) {
                    nodeProp.setNodeType(NodeType.DELIMITED);
                } else {
                    nodeProp.setNodeType(NodeType.ARRAY);
                }
                struc = (tp.length() == 2 ? "delim" : "array");
                break;
            default:
                struc = "?";
            }
            
            if (rmin.isInteger() && !rmin.isInteger(1)) {
                if (!(newElemDecl instanceof TopLevelElement)) {
                    newElemDecl.setMinOccurs(
                            BigInteger.valueOf(((MonkInteger) rmin).value));
                }
            }
            if (rmax.isInteger() && !rmax.isInteger(1)) {
                if (!(newElemDecl instanceof TopLevelElement)) {
                    long max = ((MonkInteger) rmax).value;
                    if (max == -1) {
                        newElemDecl.setMaxOccurs("unbounded");
                    } else {
                        newElemDecl.setMaxOccurs(BigInteger.valueOf(max));
                    }
                }
            }
            if (offs.isInteger()) {
                if (nodeProp.getNodeType().equals(NodeType.FIXED_LENGTH)) {
                    nodeProp.setOffset(((MonkInteger) offs).value);
                }
            } else if (!offs.isSymbol("und")) {
                // Huh?
                die(offs, "unsupported field offset format (" + offs.typeName()
                        + ")");
            }

            // Figure out the length.
            if (tlen.isInteger()) {
                // Simple numeric length.
                /*-
                 if (debug)
                 System.out.println("[ length is " + tlen.typeName() + " ]");
                 -*/
                long len = ((MonkInteger) tlen).value;

                if (len > Integer.MAX_VALUE) {
                    die(tlen, "length is too large");
                }
                if (nodeProp.getNodeType().equals(NodeType.FIXED_LENGTH)) {
                    nodeProp.setLength((int) len);
                }
            } else if (tlen.isPair()) {
                // Newer notation for encoded length: list (from, size).
                MonkData from = tlen.getCar();
                MonkData size = tlen.getCdr().getCar();
                if (!from.isInteger() || !size.isInteger()) {
                    die(tlen, "expected two integers for encoded length, not "
                            + from.typeName() + " and " + size.typeName());
                }
                long lenoff = ((MonkInteger) from).value;
                if (lenoff > Integer.MAX_VALUE) {
                    die(tlen, "length offset is too large");
                }

                long lensiz = ((MonkInteger) size).value;
                if (lensiz > Integer.MAX_VALUE) {
                    die(tlen, "length size is too large");
                }

                if (lenoff < 0 || lensiz < 1) {
                    die(tlen, "invalid values for encoded length (" + lenoff
                            + ", " + lensiz + ")");
                }
                if (lenoff != 0) {
                    if (nodeProp.getNodeType().equals(NodeType.FIXED_LENGTH)) {
                        nodeProp.setPosition(lenoff);
                    }
                }
                if (nodeProp.getNodeType().equals(NodeType.FIXED_LENGTH)) {
                    nodeProp.setLength((int) lensiz);
                }
            } else if (tlen.isString()) {
                // Can only be encoded length: "[" number "]".
                String s = tlen.getString();
                int len = s.length();
                if (len < 3 || s.charAt(0) != '[' || s.charAt(len - 1) != ']') {
                    die(tlen, "expected brackets for encoded length [" + s
                            + ']');
                }
                s = s.substring(1, len - 1);
                try {
                    // Try to parse the number.
                    int lensiz = Integer.parseInt(s);
                    if (lensiz <= 0) {
                        die(tlen, "illegal encoded length value [" + s + ']');
                    }
                    //Should set position to 0?
                    if (nodeProp.getNodeType().equals(NodeType.FIXED_LENGTH)) {
                        nodeProp.setPosition(0);
                        nodeProp.setLength((int) lensiz);
                    }
                } catch (NumberFormatException ne) {
                    die(tlen, "encoded length not a number [" + s + "]: "
                            + ne.getMessage());
                }
            } else if (!tlen.isSymbol("und")) {
                // Probably encoded length, which we don't support yet...
                die(tlen, "unsupported field length format (" + tlen.typeName()
                        + ")");
            }

            if (itag.isString() && ((MonkString) itag).length > 0) {
                // NYI: should switch if "ntag" is set
                String regex = new String(((MonkString) itag).value);
                regex = convRegex(Misc.escapeString(regex));
                nodeProp.setMatch(Misc.escapeString(regex));
                nodeProp.setAlignment(Alignment.REGEX);
            }
            if (otag.isString()) {
                /*
                 * The "output tag" is the 6th part of an SSC field line. It
                 * represents the default output data to be emitted when a field
                 * nor its children have been explicitly set. In Monk, we
                 * require the original byte-sequence from the string literal
                 * reprsentation in the SSC source file. In XSC, we want to
                 * present both the character translation of those bytes as text
                 * (for user information), and the original bytes with their
                 * encoding type, so we can reproduce the byte sequence in the
                 * normalised output-SSC.
                 */
                MonkString oval = (MonkString) otag;
                if (oval.length > 0) {
                    /*- e.part(" defaultValue=\"" + sscNormal(oval) + '"'); -*/
                    newElemDecl.setDefault(
                            Misc.escapeString(new String(oval.value)));
                    byte[] data = null;
                    try {
                        data = StringCoderFactory.getStringCoder(
                                GenSsc.monk2javaCode(oval.code)).encode(
                                otag.getString());
                    } catch (UnsupportedEncodingException ue) {
                        throw new RuntimeException("cannot recode default "
                                + "as [" + oval.code + "]");
                    }
                    boolean sameAsText = false;
                    if (data.length == oval.length) {
                        sameAsText = true;
                        for (int i = 0; i < oval.length; i++) {
                            if ((char) (data[i] & 0xFF) != oval.value[i]) {
                                sameAsText = false;
                                break;
                            }
                        }
                    }
                    if (defBytes || !sameAsText) {
                        // Byte sequence differs from Unicode equivalent.
                        /*- e.part(" defaultBytes=\""
                                + sscNormal(GenXsc.byteString(oval.bytes))
                                + '"'); -*/
                        newElemDecl.setDefault(
                                Misc.escapeString(
                                        GenXsc.byteString(oval.bytes)));
                    }
                    if (oval.code != null
                            && (defBytes
                                    || !oval.code.equals(sscEncoding))) {
                        // Needs literal prefix in SSC.
                        //TODO: Handle encoding for default value
                        noSupport(oval, NoSupport.ENCODING_OF_DEFAULT_VAL);
                        /*- e.part(" defaultEncoding=\""
                                + GenSsc.monk2javaCode(oval.code) + '"'); -*/
                    }
                }
            }

            if (scav != null) {
                /*- e.part(" scavenger=\"" + sscNormal((MonkString) scav) + '"'); -*/
                Scvngr scv = nodeProp.addNewScvngr();
                scv.setChars(
                        Misc.escapeString(
                                new String(((MonkString) scav).value)));
                if (sout) {
                    /*- e.part(" scavOutput=\"true\""); -*/
                    scv.setEmit1St(true);
                }
            }
            if (cmin != null) {
                // - assert(cmax != null);
                /*- e.part(" childMin=\"" + ((MonkInteger) cmin).value + '"');
                e.part(" childMax=\""
                        + (cmax.isInteger() ? Integer
                                .toString((int) ((MonkInteger) cmax).value)
                                : "unbounded") + '"'); -*/
                NOfN nofn = nodeProp.addNewNOfN();
                nofn.setMinN((int) ((MonkInteger) cmin).value);
                if (cmax.isInteger()) {
                    nofn.setMaxN((int) ((MonkInteger) cmax).value);
                } else {
                    nofn.setMaxN(-1);
                }
            }

            if (exac) {
                /*- e.part(" exact=\"true\""); -*/
            } else {
                if (top) {
                    rootNodeProp.setUndefDataPolicy(UndefDataPolicy.MAP);
                }
            }
            if (grpd) {
                nodeProp.setOrder(Order.ANY);
            }
            if (ntag) {
                /*- e.part(" avoidMatch=\"true\""); -*/
                nodeProp.setNoMatch(true);
            }
            if (prec) {
                /*- e.part(" precedence=\"parent\""); -*/
                noSupport(precData, NoSupport.PARENT_PRECEDENCE);
            }
            
            for (Delimiter d : normalDelims) {
                d.setKind(Kind.NORMAL);
                d.setOptionalMode(optionalMode);
                d.setTerminatorMode(termMode);
                if (!beginAnch) {
                    d.setBeginAnch(false);
                }
                if (!endAnch) {
                    d.setEndAnch(false);
                }
            }

            //Handle delimiter of fixed length field
            if (NodeType.FIXED_LENGTH.equals(nodeProp.getNodeType())
                    && delimSet.sizeOfLevelArray() > 0
                    && delimSet.getLevelArray(0).sizeOfDelimiterArray() > 0) {
                if (delimSet.sizeOfLevelArray() > 1) {
                    noSupport(name, NoSupport.MULTI_DELIM_LEVEL_OF_FIXED);
                }
                if (delimSet.getLevelArray(0).sizeOfDelimiterArray() > 1) {
                    noSupport(name, NoSupport.MULTI_DELIM_OF_FIXED);
                }
                delim = delimSet.getLevelArray(0).getDelimiterArray(0);
                if (delim.isSetBytes()) {
                    noSupport(name, NoSupport.END_DELIM_OF_FIXED);
                }
                if (delim.isSetBeginBytes()) {
                    if (!delim.getBeginBytes().isSetConstant()) {
                        noSupport(name, NoSupport.EMBEDDED_DELIM_OF_FIXED);
                    } else {
                        nodeProp.addNewDelimOfFixed();
                        nodeProp.getDelimOfFixed().setBeginBytes(
                                delim.getBeginBytes().getConstant());
                        nodeProp.getDelimOfFixed().setBeginAnch(
                                delim.getBeginAnch());
                    }
                }
                delimSet.setLevelArray(new DelimiterLevel[0]);
            }
            
            // Emit all the subfields.
            boolean brood = false;
            for (; rest.isPair(); rest = ((MonkPair) rest).cdr) {
                // Subordinate <node> entities.
                brood |= convField(schema, newElemDecl, ((MonkPair) rest).car,
                        depth + 1, rootNodeProp);
            }
            
            if (!isParent) {
                newElemDecl.setType(
                        new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "string"));
            }
            
        } else if (tp.equals("LTN") || tp.equals("LTF") || tp.equals("LTS")
                || tp.equals("GTN") || tp.equals("GTF") || tp.equals("GTS")) {
            // Template reference.
            String struc;
            switch (tp.charAt(2)) {
            case 'F':
                struc = "fixed";
                break;
            case 'N':
                struc = "delim";
                break;
            case 'S':
                struc = "set";
                break;
            default:
                struc = "?";
            }
            String rf = "REFERENCE";
            String rs = null;
            String rm = null;
            if (tp.charAt(0) == 'L') {
                // Local template reference.
                if (!otag.isSymbol()) {
                    die(otag, "local reference requires symbol");
                }
                rm = otag.getSymbol();
                if (rm.endsWith("-struct"))
                    rm = rm.substring(0, rm.length() - 7);
            } else {
                // Global template reference.
                if (!itag.isString()) {
                    die(itag, "global reference requires string");
                }
                rs = normalizePath(itag, new String(((MonkString) itag).value));
                rm = otag.getSymbol();
                if (rm.endsWith("-struct"))
                    rm = rm.substring(0, rm.length() - 7);
            }
            QName refName;
            if (targetNamespace != null && targetNamespace.length() > 0) {
                refName = new QName(targetNamespace, rm);
            } else {
                refName = new QName(rm);
            }
            newElemDecl.setRef(refName);
            if (rmin.isInteger() && !rmin.isInteger(1)) {
                newElemDecl.setMinOccurs(
                        BigInteger.valueOf(((MonkInteger) rmin).value));
            }
            if (rmax.isInteger() && !rmax.isInteger(1)) {
                long max = ((MonkInteger) rmax).value;
                if (max == -1) {
                    newElemDecl.setMaxOccurs("unbounded");
                } else {
                    newElemDecl.setMaxOccurs(BigInteger.valueOf(max));
                }
            }
        } else {
            // Unknown type?
            die(type, "unrecognised field type [" + type.value + "]");
        }
        
        if (!newElemDecl.isSetRef()) {
            //Remove unused delimiter set
            if (delimSet.sizeOfLevelArray() == 1
                    && delimSet.getLevelArray(0).sizeOfDelimiterArray() == 0) {
                delimSet.removeLevel(0);
            }
            if (delimSet.sizeOfLevelArray() == 0) {
                nodeProp.unsetDelimiterSet();
            }
            Annotation anno = newElemDecl.addNewAnnotation();
            anno.addNewAppinfo().set(customEncoding);
        }
        
        return true;
    }

    /**
     * Generates the begin delimiter from a Monk string.
     * 
     * @param str Monk string literal token
     */
    public void makeBeginDelim(MonkData str, Delimiter delim) {
        if (str == null || !str.isString())
            throw new IllegalArgumentException(
                    "makeEndDelim: non-string input");
        MonkString ms = (MonkString) str;
        DelimiterBytes bytes = delim.addNewBeginBytes();
        bytes.setConstant(
                ms.value == null ? null
                        : Misc.escapeString(new String(ms.value)));
    }

    /**
     * Generates the end delimiter from a Monk string.
     * 
     * @param str Monk string literal token
     */
    public void makeEndDelim(MonkData str, Delimiter delim) {
        if (str == null || !str.isString())
            throw new IllegalArgumentException(
                    "makeEndDelim: non-string input");
        MonkString ms = (MonkString) str;
        DelimiterBytes bytes = delim.addNewBytes();
        bytes.setConstant(
                ms.value == null ? null
                        : Misc.escapeString(new String(ms.value)));
    }

    private ExplicitGroup getGroup(Element elemDecl) {
        if (!elemDecl.isSetComplexType()) {
            elemDecl.addNewComplexType();
        }
        LocalComplexType localType = elemDecl.getComplexType();
        ExplicitGroup group;
        if (localType.isSetAll()) {
            group = localType.getAll();
        } else if (localType.isSetSequence()) {
            group = localType.getSequence();
        } else if (localType.isSetChoice()) {
            group = localType.getChoice();
        } else {
            group = localType.addNewSequence();
        }
        return group;
    }
    
    /**
     * Given a filename, as used in an input SSC file, finds it along the Monk
     * load path specified. Registers the mapping found in the global "loads"
     * mapping. Returns the path relative to the input SSC root, or null if not
     * found.
     * <p>
     * SSC files contain a "load" statement for each external template. Note
     * that they follow any set-file-encoding-method calls in the SSC files
     * generated by the Monk ETD editor, but we ignore the encoding for now...
     * <p>
     * If the global "dumbSscRoot" is set, then just prefix this to translate
     * the reference.
     * 
     * @param ssc
     *            the unnormalized SSC file path
     */
    private String normalizeLoad(String ssc) {
        String encoder;
        if (loads == null) {
            loads = new HashMap<String, String>();
        }

        // Really look it up.
        if (sscSearch.length == 0) {
            throw new RuntimeException(
                    "no load-path to resolve reference [" + ssc + "]");
        }
        encoder = (String) loads.get(ssc);
        for (int i = 0; encoder == null && i < sscSearch.length; i++) {
            File dir = new File(sscInputRoot, sscSearch[i]);
            if ((new File(dir, ssc)).exists()) {
                String part = sscSearch[i];
                if (part.equals("")) {
                    part = null;
                }
                encoder = slashPath(new File(part, ssc));
                while (encoder.startsWith("./")) {
                    encoder = encoder.substring(2);
                }
                break;
            }
        }
        if (encoder == null) {
            // Not found.
            return null;
        }
        loads.put(ssc, encoder);
        if (debug) {
            System.out.println("[ load <" + ssc + "> = <" + encoder + "> ]");
        }
        return encoder;
    }

    /**
     * Converts load-path name to normalized load path, or null if not found. We
     * expect the global list "loads" to map internal path references to
     * normalized references (both strings). Strips the usual ".ssc" suffix.
     * 
     * @param data
     *            for input line reference
     * @param ssc
     *            the unnormalized SSC file path
     */
    private String normalizePath(MonkData data, String path) {
        if (debug) {
            System.out.println("[ normalise <" + path + "> ]");
        }
        String result = null;
        if (loads != null)
            result = (String) loads.get((Object) path);
        if (result == null)
            die(data, "using file reference \"" + path
                    + "\" without prior load clause");
        if (result.endsWith(".ssc"))
            result = result.substring(0, result.length() - 4);
        return result;
    }
    
    /**
     * Class to match Monk data with a pattern. This is used for analysis of
     * Monk data, after reading it in from a file as generic Monk source.
     */
    public static class MonkMatch {
        private String desc;

        private MonkData[] out = null;

        private String what;

        private int lastIndex = -1;

        /**
         * Constructs a new match pattern.
         * 
         * @param what
         *            verbal description, for error message
         * @param desc
         *            match pattern
         * @param oargs
         *            output argument count
         */
        public MonkMatch(String what, String desc, int oargs) {
            this.what = what;
            this.desc = desc;
            this.out = (oargs > 0 ? new MonkData[oargs] : null);
        }

        /**
         * Reports the number of output arguments found by the match.
         * 
         * @return a non-negative integer value
         */
        public int outMax() {
            return (out == null ? 0 : out.length);
        }

        /**
         * Gets the Nth output argument found by the match. Arguments are
         * numbered from 0.
         * 
         * @param the
         *            index value
         * @return the item of Monk data
         */
        public MonkData out(int i) {
            if (i < 0 || outMax() <= i)
                throw new IndexOutOfBoundsException("no out(" + i
                        + "), range is 0 through " + (outMax() - 1));
            return out[i];
        }

        /**
         * Returns the index of the last matched item. -1 indicates an empty
         * list.
         * 
         * @return the index (starting at 0)
         */
        public int getLastIndex() {
            return this.lastIndex;
        }
        
        /**
         * Matches a Monk list to the descriptor. Returns the matched items via
         * the "out" array. If not matched, returns non-null, or throws
         * exception if the "must" flag is set. The elements are matched as
         * follows:
         * 
         * B = boolean, I = integer, L = pair, S = string, Y = symbol, X =
         * anyting, * = rest of list.
         * 
         * When an element is preceded by '?' it is optional; if preceded by '-'
         * then skip it.
         * 
         * @param must
         *            flag: must match or fail?
         * @param data
         *            input list to match
         * @param rarg
         *            array of input arguments
         * @return error message, or null if okay
         * @throws IllegalArgumentException
         *             on mismatch, if "must" set
         */
        private String match(boolean must, MonkData data, MonkData[] rarg)
                throws IllegalArgumentException {
            int opos = 0;
            this.lastIndex = -1;
            boolean opt = false, skip = false;
            byte type = MonkData.NONE;
            int amax = (desc == null ? -1 : (desc.length() - 1));
            int elem = 0;
            MonkData a = null;
            for (int i = 0; i < desc.length(); i++) {
                char c = desc.charAt(i);
                switch (c) {
                case '?':
                    // Next is optional.
                    opt = true;
                    continue;
                case '-':
                    // Next is skipped.
                    skip = true;
                    continue;

                case 'B':
                    type = MonkData.BOOLEAN;
                    break;
                case 'I':
                    type = MonkData.INTEGER;
                    break;
                case 'L':
                    type = MonkData.PAIR;
                    break;
                case 'S':
                    type = MonkData.STRING;
                    break;
                case 'Y':
                    type = MonkData.SYMBOL;
                    break;
                case 'X':
                    type = MonkData.NONE;
                    break;

                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    // Match particular item.
                    int anum = c - '0';
                    if (anum > amax)
                        throw new RuntimeException("no input arg #" + anum);
                    a = rarg[anum];
                    type = a.type;
                    break;

                case '*':
                    // Rest of list.
                    if (!skip) {
                        out[opos] = data;
                        this.lastIndex = opos++;
                    }
                    continue;

                case ')':
                    // End of list.
                    if (!data.isNil()) {
                        if (must)
                            die(data, what + ": trailing junk in list");
                        return "trailing junk in list";
                    }
                    continue;

                default:
                    // Huh?
                    throw new RuntimeException("MonkMatch: <" + c + "> ?");
                }
                // Expecting data of particular type.
                if (data.isPair()) {
                    MonkData m = data.getCar();
                    if ((type == MonkData.NONE || m.type == type)
                            && (a == null || monkEquals(m, a))) {
                        // We have a match.
                        if (!skip) {
                            // Yield as output.
                            if (opos >= outMax())
                                throw new RuntimeException("MonkMatch: "
                                        + "over " + outMax() + " outputs in "
                                        + what);
                            out[opos] = m;
                            this.lastIndex = opos++;
                        }
                        /*-
                        if (debug) {
                            System.out.println("[ consume " + elem
                                + " as <" + c + "> ]");
                        }
                        -*/
                        data = data.getCdr();
                        elem++;
                    } else if (opt) {
                        // Optional element missing.
                        if (!skip) {
                            // Yield as output.
                            if (opos >= outMax())
                                throw new RuntimeException("MonkMatch: "
                                        + "over " + outMax() + " outputs in "
                                        + what);
                            out[opos] = null;
                            this.lastIndex = opos++;
                        }
                        /*-
                        if (debug) {
                            System.out.println("[ missing " + elem
                                + " as <" + c + "> ]");
                        }
                        -*/
                    } else {
                        // Data mismatched.
                        if (!must) {
                            return "type mismatch";
                        }
                        die(data, "match " + what + ": expected "
                                + MonkData.typeName(type) + ", but element "
                                + elem + " is " + m.typeName());
                    }
                } else if (!opt) {
                    // Data structure is not a proper list (pair chain).
                    if (must) {
                        if (data.isSymbol()) {
                            die(data, "extra element named "
                                    + ((MonkSymbol) data).value
                                    + " encountered.");

                        } else {
                            die(
                                    data,
                                    "match "
                                            + what
                                            + ": invalid element"
                                            + " of type "
                                            + data.typeName()
                                            + " as list tail, expected a list element (type "
                                            + c + ")");
                        }
                    }
                    return "not a list";
                }
                opt = skip = false;
            }
            return null;
        }
    }
    
    public static class NoSupport {
        
        /**
         * Multiple escape sequences.
         */
        public static final int MULTIPLE_ESC_SEQUENCES = 1;
        
        /**
         * Encoding of default value.
         */
        public static final int ENCODING_OF_DEFAULT_VAL = 2;
        
        /**
         * Parent precedence is not automatically supported. Requires manual
         * delimiter precedence adjustment after conversion is done.
         */
        public static final int PARENT_PRECEDENCE = 3;  
        
        /**
         * Multiple delimiter levels for fixed length field.
         */
        public static final int MULTI_DELIM_LEVEL_OF_FIXED = 4;  
        
        /**
         * Multiple delimiters for fixed length field.
         */
        public static final int MULTI_DELIM_OF_FIXED = 5;  
        
        /**
         * End delimiter for fixed length field.
         */
        public static final int END_DELIM_OF_FIXED = 6;  
        
        /**
         * Embedded delimiter for fixed length field.
         */
        public static final int EMBEDDED_DELIM_OF_FIXED = 7;  
        
        private static final int START = MULTIPLE_ESC_SEQUENCES;
        private static final int END = EMBEDDED_DELIM_OF_FIXED;
        
        private final int _code;
        private final String _msg;
        
        public NoSupport(int code, String msg) {
            if (code < START && code > END) {
                throw new IndexOutOfBoundsException("code=" + code);
            }
            _code = code;
            if (msg == null) {
                _msg = "";
            } else {
                _msg = msg;
            }
        }
        
        @Override
        public String toString() {
            switch (_code) {
            case MULTIPLE_ESC_SEQUENCES:
                return "Multiple definitions of escape sequence"
                        + " is not supported. " + _msg;
            case ENCODING_OF_DEFAULT_VAL:
                return "Encoding of default value is not supported. " + _msg;
            case PARENT_PRECEDENCE:
                return "Since the SSC uses Pp (parent precedence) flag, "
                        + "manual delimiter precedence adjustment "
                        + "may be needed. " + _msg;
            case MULTI_DELIM_LEVEL_OF_FIXED:
                return "Multiple delimiter levels in fixed length field"
                        + " is not supported. " + _msg;
            case MULTI_DELIM_OF_FIXED:
                return "Multiple delimiters in fixed length field"
                        + " is not supported. " + _msg;
            case END_DELIM_OF_FIXED:
                return "End delimiter in fixed length field"
                        + " is not supported. " + _msg;
            case EMBEDDED_DELIM_OF_FIXED:
                return "Embedded delimiter in fixed length field"
                        + " is not supported. " + _msg;
            }
            return "Unknown code=" + _code;
        }
    }
    
    /**
     * Converts input SSC files to output XSD files. Return non-zero status if
     * conversion failed.
     * 
     * Usage: main [-option arg] file.ssc ...
     * 
     * @param args
     *            the command-line arguments
     */
    public static void main(String[] args) {
        File idir = new File(".");
        File odir = new File(".");
        List<String> loadv = new ArrayList<String>();
        String targetNamespace = "";
        StringBuffer sx = new StringBuffer();
        Set<String> ext = new HashSet<String>();
        boolean uconv = false;
        boolean debug = false;
        int c = 0;

        Option opt = new Option(args, "d:i:o:l:t:m:x:");
        opt.debug = true;
        while ((c = opt.getOpt()) != Option.EOF) {
            switch (c) {
            case 'd':
                // Switch on debugging.
                debug = true;
                break;
            case 'i':
                // Input root directory.
                idir = new File(opt.getOptArg());
                break;
            case 'o':
                // Output root directory.
                odir = new File(opt.getOptArg());
                break;
            case 'l':
                // Load-path element, relative to input root; may be empty
                // (meaning input root itself).
                loadv.add(opt.getOptArg());
                break;
            case 't':
                // Target namespace
                targetNamespace = opt.getOptArg();
                break;
            case 'u':
                // Convert non-ASCII to U-escapes in Java names.
                // (only works with -j).
                uconv = true;
                break;
            }
        }
        // Copy "-l" args to loads[].
        String[] loads = loadv.toArray(new String[0]);

        // Copy remaining arguments to sscs[].
        int oind = opt.getOptInd();
        String[] sscs = new String[args.length - oind];
        for (int i = 0; i < sscs.length; i++)
            sscs[i] = args[i + oind];

        if (sscs.length < 1) {
            // Que?
            System.err.println("Usage: "
                    + "ssc2custom [-i indir] [-o outdir] [-l loaddir] "
                    + "[-t targetnamespace] " + "file.ssc ...");
            System.err.println("Options include:");
            System.err.println("    -i indir directory where input .ssc files reside");
            System.err.println("    -o outdir directory where output Encoder XSD files reside");
            System.err.println("    -l loaddir directory (relative to the input directory) where external template files reside");
            System.err.println("    -t targetnamespace target namespace applied to all Encoder XSD files. There is no ");
            System.err.println("        way to specify different target namespaces for different files. If target namespace is not specified, all Encoder XSD files will have no target namespace.");
            System.err.println();
            System.err.println("    file.ssc ... specify the .ssc files that need to be converted. File name wildcard is not supported. File name must be relative to the input directory.");
            System.err.println();
            System.err.println("    Any external templates referenced will be converted as well.");
            System.exit(1);
        }
        try {
            Ssc2Custom.debug = debug;
            Set<NoSupport> notSupported = new HashSet<NoSupport>();
            Ssc2Custom.convert(idir, odir, loads, sscs, targetNamespace,
                    uconv, sx, ext, notSupported);
            for (Iterator<String> i = ext.iterator(); i.hasNext();) {
                sx.append("EXTERNAL TEMPLATES: " + i.next() + '\n');
            }
            for (NoSupport nos : notSupported) {
                sx.append("WARNING: ").append(nos).append('\n');
            }
            System.out.print(sx.toString());
        } catch (Exception exc) {
            exc.printStackTrace();
            System.exit(1);
        }
        System.exit(0);
    }
}
