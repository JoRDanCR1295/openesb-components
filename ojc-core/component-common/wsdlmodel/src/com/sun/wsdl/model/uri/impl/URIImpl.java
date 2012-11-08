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
 * @(#)URIImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.uri.impl;

import com.sun.wsdl.model.uri.URI;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * Implementation of URI (Java 1.4.2 version does NOT work correctly).
 *
 * @author Sun Microsystems
 * @version 
 */
public class URIImpl extends URI {
    
    /** Holds the URI (converted to forward slashes) */
    private String legalURI;
    
    /** Holds the scheme */
    private String scheme;
    
    /** Holds whether URI is absolute. */
    private boolean absolute = false;
    
    /** Holds the base directory */
    private String directory;
    
    /** Holds the filename */
    private String filename;
    
    /** Holds opaque flag.  Anything other than http: or file: is considered opaque for this
     * implementation.
     */
    private boolean opaque;
    
    /** Constructor.
     */
    public URIImpl() {
        super();
    }
    
    /** Constructor for a non-opaque URIImpl.
     * @param   scheme      Scheme.
     * @param   absolute    Whether URI is absolute.
     * @param   directory   Directory
     * @param   filename    Filename.
     */
    public URIImpl(String scheme, boolean absolute, String directory, String filename) {
        super();
        this.scheme = scheme.toLowerCase();
        this.absolute = absolute;
        this.directory = directory;
        this.filename = filename;
        this.opaque = false;
    }
    
    /** Constructor for a URI.
     * @param   str     URI string.
     * @return  URI corresponding to string.
     */
    public URIImpl(String str) {
        super();
        legalURI = str.replace('\\', '/');
        int colon = smartScan(legalURI, 0, legalURI.length(), "/?#", ":");
        scheme = (colon != -1 ? legalURI.substring(0, colon) : "file");
        if (isDOSDrive(scheme)) {
            scheme = "file";
            colon = -1;
        }
        scheme = scheme.toLowerCase();
        
        opaque = !("file".equals(scheme) || "http".equals(scheme));
        if (!opaque) {
            // parse the directory from the filename
            int lastSlash = legalURI.lastIndexOf('/');
            if (lastSlash != -1) {
                directory = legalURI.substring(colon + 1, lastSlash);
                
                // if there's only one slash in the legal URI, then need to add back a slash to the directory
                if (directory.lastIndexOf('/') == -1) {
                    directory += '/';
                }
                
                // parse out the filename
                if (legalURI.length() > (lastSlash + 1)) {
                    filename = legalURI.substring(lastSlash + 1);
                }
            } else {
                filename = legalURI.substring(colon + 1);
            }
            
            if (directory != null) {
                if (directory.startsWith("/")) {
                    absolute = true;
                    // Check if there's a DOS drive here
                    int dosColon = smartScan(directory, 1, directory.length(), "/?#", ":");
                    if (dosColon != -1) {
                        String drive = directory.substring(1, dosColon);
                        if (isDOSDrive(drive)) {
                            absolute =
                                (((dosColon + 1) < directory.length()) && (directory.charAt(dosColon + 1) == '/'));
                            directory = directory.substring(1);
                        }
                    }
                } else {
                    // Check if there's a DOS drive here
                    int dosColon = smartScan(directory, 0, directory.length(), "/?#", ":");
                    if (dosColon != -1) {
                        String drive = directory.substring(0, dosColon);
                        if (isDOSDrive(drive)) {
                            absolute =
                                (((dosColon + 1) < directory.length()) && (directory.charAt(dosColon + 1) == '/'));
                        }
                    }
                }
            }
        } else {
            absolute = true;
        }
    }
    
    /** @see com.sun.wsdl.model.uri.URI#getScheme()
     */
    public String getScheme() {
        return scheme;
    }
    
    /** @see com.sun.wsdl.model.uri.URI#getDirectory()
     */
    public String getDirectory() {
        return (directory != null ? directory : ".");
    }
    
    /** @see com.sun.wsdl.model.uri.URI#getFilename()
     */
    public String getFilename() {
        return (filename != null ? filename : "");
    }
    
    /** @see com.sun.wsdl.model.uri.URI#isOpaque()
     */
    public boolean isOpaque() {
        return opaque;
    }
    
    /** @see com.sun.wsdl.model.uri.URI#isAbsolute()
     */
    public boolean isAbsolute() {
        return absolute;
    }
    
    /** @see com.sun.wsdl.model.uri.URI#resolve(java.lang.String)
     */
    public URI resolve(String str) {
        URI ru = new URIImpl(str);
        return resolve(ru);
    }
    
    /** @see com.sun.wsdl.model.uri.URI#resolve(com.sun.wsdl.model.uri.URI)
     */
    public URI resolve(URI uri) {
        URI ru = uri;
        if (ru.isOpaque() || ru.isAbsolute()) {
            return ru;
        }
        String ndir = httpFixRootDirectory(normalizePath(getDirectory() + "/" + ru.getDirectory()));
        return new URIImpl(getScheme(), isAbsolute(), ndir, ru.getFilename());
    }
    
    /** @see com.sun.wsdl.model.uri.URI#normalize()
     */
    public URI normalize() {
        if (isOpaque()) {
            return this;
        }
        String ndir = httpFixRootDirectory(normalizePath(getDirectory()));
        return new URIImpl(getScheme(), isAbsolute(), ndir, getFilename());
    }

    /** @see com.sun.wsdl.model.uri.URI#relativize(java.lang.String)
     */
    public URI relativize(String str) {
        URI ru = new  URIImpl(str);
        return relativize(ru);
    }
    
    /** @see com.sun.wsdl.model.uri.URI#relativize(com.sun.wsdl.model.uri.URI)
     */
    public URI relativize(URI uri) {
        URI ru = uri;
        if (ru.isOpaque() || isOpaque()) {
            return ru;
        }
        if (!getScheme().equals(ru.getScheme())) {
            return ru;
        }
        
        String baseDir = normalizePath(getDirectory());
        if ((baseDir.length() > 0) && (baseDir.charAt(baseDir.length() - 1) != '/')) {
            baseDir += "/";
        }
        
        String targetDir = normalizePath(ru.getDirectory());
        if ((targetDir.length() > 0) && (targetDir.charAt(targetDir.length() - 1) != '/')) {
            targetDir += "/";
        }

        String relDir = ".";
        
        // Target is a direct descendant
        if (targetDir.startsWith(baseDir)) {
            relDir = targetDir.substring(baseDir.length());
            if (relDir.length() == 0) {
                relDir = ".";
            }
        } else {
            // Target is not direct descendant, find common parent
            int commonDirPosn = matchIndexOf(baseDir, targetDir);
            if ((commonDirPosn != -1) && (baseDir.charAt(commonDirPosn) == '/')) {
                // Count generational difference in base
                int gen = 0;
                for (int i = (commonDirPosn + 1);
                        ((i = baseDir.indexOf('/', i)) != -1); i++) {
                    gen++;
                }
                StringBuffer relDirSb = new StringBuffer();
                for (int i = 0; i < gen; i++) {
                    relDirSb.append("../");
                }
                relDirSb.append(targetDir.substring(commonDirPosn + 1));
                relDir = relDirSb.toString();
            } else {
                return ru;
            }
        }
        ru = new URIImpl("file", false, relDir, ru.getFilename());
        return ru;
    }
    /** @see com.sun.wsdl.model.uri.URI#toURL()
     */
    public URL toURL() throws MalformedURLException {
        return new URL(toString());
    }
    
    /** @see com.sun.wsdl.model.uri.URI#toFile()
     */
    public File toFile() {
        File rFile = null;
        if ("file".equals(getScheme())) {
            rFile = new File(getDirectory().replace('/', File.separatorChar),
                             getFilename().replace('/', File.separatorChar));
        }
        return rFile;
    }
    
    /** @see java.lang.Object#toString()
     */
    public String toString() {
        return (opaque ? legalURI : (getScheme() + ":" + dirConcatFile(getDirectory(), getFilename())));
    }
    
    // Following until (***) is courtesy Sun JDK 1.4 java.net.URI which is not part of JDK 1.3

	/** Scan forward from the given start position.  Stop at the first char
	 * in the err string (in which case -1 is returned), or the first char
	 * in the stop string (in which case the index of the preceding char is
	 * returned).
     *
     * @param   str         String to scan.
     * @param   start       Starting position to scan.
     * @param   end         Ending position (exclusive) to scan.
     * @param   err         String of character which aborts the scan.
     * @param   stop        String to scan for.
     * @return  The position of the scheme colon or <code>-1</code> if not found.
	 */
	private int smartScan(String str, int start, int end, String err, String stop) {
	    int p = start;
        while (p < end) {
            char c = str.charAt(p);
            if (err.indexOf(c) >= 0) {
                return -1;
            }
            if (stop.indexOf(c) >= 0) {
                return p;
            }
            p++;
        }
	    return -1;
	}
    
    /** Normalizes a file path to get rid of .. and .
     * @param   ps  Path to be normalized.
     * @return  Normalized path.
     */
    private String normalizePath(String ps) {
        
        // Does this path need normalization?
        int ns = needsNormalization(ps);        // Number of segments
        if (ns < 0) {
            // Nope -- just return it
            return ps;
        }
        
        char[] path = ps.toCharArray();         // Path in char-array form
        
        // Split path into segments
        int[] segs = new int[ns];               // Segment-index array
        split(path, segs);
        
        // Remove dots
        removeDots(path, segs);
        
        // Prevent scheme-name confusion
        maybeAddLeadingDot(path, segs);
        
        // Join the remaining segments and return the result
        return new String(path, 0, join(path, segs));
    }
    
    /**
     * The following algorithm for path normalization avoids the creation of a
     * string object for each segment, as well as the use of a string buffer to
     * compute the final result, by using a single char array and editing it in
     * place.  The array is first split into segments, replacing each slash
     * with '\0' and creating a segment-index array, each element of which is
     * the index of the first char in the corresponding segment.  We then walk
     * through both arrays, removing ".", "..", and other segments as necessary
     * by setting their entries in the index array to -1.  Finally, the two
     * arrays are used to rejoin the segments and compute the final result.
     *
     * This code is based upon src/solaris/native/java/io/canonicalize_md.c
     * Check the given path to see if it might need normalization.  A path
     * might need normalization if it contains duplicate slashes, a "."
     * segment, or a ".." segment.  Return -1 if no further normalization is
     * possible, otherwise return the number of segments found.
     *
     * This method takes a string argument rather than a char array so that
     * this test can be performed without invoking path.toCharArray().
     *
     * @param path  Path to determine if normalization is needed.
     * @return  The number of segments to be normalized or -1 if normalized already.
     */
    private int needsNormalization(String path) {
        boolean normal = true;
        int ns = 0;                     // Number of segments
        int end = path.length() - 1;    // Index of last char in path
        int p = 0;                      // Index of next char in path
        
        // Skip initial slashes
        while (p <= end) {
            if (path.charAt(p) != '/') {
                break;
            }
            p++;
        }
        if (p > 1) {
            normal = false;
        }
        
        // Scan segments
        while (p <= end) {
            
            // Looking at "." or ".." ?
            if ((path.charAt(p) == '.')
                    && ((p == end)
                    || ((path.charAt(p + 1) == '/')
                    || ((path.charAt(p + 1) == '.')
                    && ((p + 1 == end)
                    || (path.charAt(p + 2) == '/')))))) {
                normal = false;
            }
            ns++;
            
            // Find beginning of next segment
            while (p <= end) {
                if (path.charAt(p++) != '/') {
                    continue;
                }
                
                // Skip redundant slashes
                while (p <= end) {
                    if (path.charAt(p) != '/') {
                        break;
                    }
                    normal = false;
                    p++;
                }
                
                break;
            }
        }
        
        return normal ? -1 : ns;
    }
    
    /**
     * Split the given path into segments, replacing slashes with nulls and
     * filling in the given segment-index array.
     *
     * Preconditions:
     *   segs.length == Number of segments in path
     *
     * Postconditions:
     *   All slashes in path replaced by '\0'
     *   segs[i] == Index of first char in segment i (0 <= i < segs.length)
     *
     * @param   path    Character array containing path.
     * @param   segs    Segments.
     */
    private void split(char[] path, int[] segs) {
        int end = path.length - 1;      // Index of last char in path
        int p = 0;                      // Index of next char in path
        int i = 0;                      // Index of current segment
        
        // Skip initial slashes
        while (p <= end) {
            if (path[p] != '/') {
                break;
            }
            path[p] = '\0';
            p++;
        }
        
        while (p <= end) {
            
            // Note start of segment
            segs[i++] = p++;
            
            // Find beginning of next segment
            while (p <= end) {
                if (path[p++] != '/') {
                    continue;
                }
                path[p - 1] = '\0';
                
                // Skip redundant slashes
                while (p <= end) {
                    if (path[p] != '/') {
                        break;
                    }
                    path[p++] = '\0';
                }
                break;
            }
        }
        
        if (i != segs.length) {
            throw new InternalError();      // ASSERT
        }
    }
    
    /**
     * Join the segments in the given path according to the given segment-index
     * array, ignoring those segments whose index entries have been set to -1,
     * and inserting slashes as needed.  Return the length of the resulting
     * path.
     *
     * Preconditions:
     *   segs[i] == -1 implies segment i is to be ignored
     *   path computed by split, as above, with '\0' having replaced '/'
     *
     * Postconditions:
     *   path[0] .. path[return value] == Resulting path
     *
     * @param   path    Character array contain path.
     * @param   segs    Segments.
     * @return  Index to next path char to write.
     */
    private int join(char[] path, int[] segs) {
        int ns = segs.length;           // Number of segments
        int end = path.length - 1;      // Index of last char in path
        int p = 0;                      // Index of next path char to write
        
        if (path[p] == '\0') {
            // Restore initial slash for absolute paths
            path[p++] = '/';
        }
        
        for (int i = 0; i < ns; i++) {
            int q = segs[i];            // Current segment
            if (q == -1) {
                // Ignore this segment
                continue;
            }
            
            if (p == q) {
                // We're already at this segment, so just skip to its end
                while ((p <= end) && (path[p] != '\0')) {
                    p++;
                }
                if (p <= end) {
                    // Preserve trailing slash
                    path[p++] = '/';
                }
            } else if (p < q) {
                // Copy q down to p
                while ((q <= end) && (path[q] != '\0')) {
                    path[p++] = path[q++];
                }
                if (q <= end) {
                    // Preserve trailing slash
                    path[p++] = '/';
                }
            } else {
                throw new InternalError();      // ASSERT false
            }
        }
        
        return p;
    }
    
    /**
     * Remove "." segments from the given path, and remove segment pairs
     * consisting of a non-".." segment followed by a ".." segment.
     *
     * @param   path    Character array containing path.
     * @param   segs    Segments.
     */
    private void removeDots(char[] path, int[] segs) {
        int ns = segs.length;
        int end = path.length - 1;
        
        for (int i = 0; i < ns; i++) {
            int dots = 0;       // Number of dots found (0, 1, or 2)
            
            // Find next occurrence of "." or ".."
            do {
                int p = segs[i];
                if (path[p] == '.') {
                    if (p == end) {
                        dots = 1;
                        break;
                    } else if (path[p + 1] == '\0') {
                        dots = 1;
                        break;
                    } else if ((path[p + 1] == '.')
                                && ((p + 1 == end)
                                || (path[p + 2] == '\0'))) {
                        dots = 2;
                        break;
                    }
                }
                i++;
            } while (i < ns);
            if ((i > ns) || (dots == 0)) {
                break;
            }
            
            if (dots == 1) {
                // Remove this occurrence of "."
                segs[i] = -1;
            } else {
                // If there is a preceding non-".." segment, remove both that
                // segment and this occurrence of ".."; otherwise, leave this
                // ".." segment as-is.
                int j;
                for (j = i - 1; j >= 0; j--) {
                    if (segs[j] != -1) {
                        break;
                    }
                }
                if (j >= 0) {
                    int q = segs[j];
                    if (!((path[q] == '.')
                            && (path[q + 1] == '.')
                            && (path[q + 2] == '\0'))) {
                        segs[i] = -1;
                        segs[j] = -1;
                    }
                }
            }
        }
    }
    
    /**
     * DEVIATION: If the normalized path is relative, and if the first
     * segment could be parsed as a scheme name, then prepend a "." segment
     *
     * @param   path    Character array containing path.
     * @param   segs    Segments.
     */
    private void maybeAddLeadingDot(char[] path, int[] segs) {
        
        if (path[0] == '\0') {
            // The path is absolute
            return;
        }
        
        int ns = segs.length;
        int f = 0;              // Index of first segment
        while (f < ns) {
            if (segs[f] >= 0) {
                break;
            }
            f++;
        }
        if ((f >= ns) || (f == 0)) {
            // The path is empty, or else the original first segment survived,
            // in which case we already know that no leading "." is needed
            return;
        }
        
        int p = segs[f];
        while ((path[p] != ':') && (path[p] != '\0')) {
            p++;
        }
        if (path[p] == '\0') {
            // No colon in first segment, so no "." needed
            return;
        }
        
        // At this point we know that the first segment is unused,
        // hence we can insert a "." segment at that position
        path[0] = '.';
        path[1] = '\0';
        segs[0] = 0;
    }
    
    // (***)
    
    /** Concatenates a directory with a file.
     * @param   dir     Directory name.
     * @param   file    File name.
     * @return  Directory concatenated with file.
     */
    private String dirConcatFile(String dir, String file) {
        dir = httpFixRootDirectory(dir);
        return (dir.endsWith("/") ? (dir + file) : (dir + "/" + file));
    }
    
    /** Fixes the http root directory if needed.
     * @param   dir     Directory to be inspected.
     * @return  Fixed http root directory.
     */
    private String httpFixRootDirectory(String dir) {
        if ("http".equalsIgnoreCase(getScheme()) && !dir.startsWith("//")) {
            dir = (dir.startsWith("/")) ? ("/" + dir) : ("//" + dir);
        }
        return dir;
    }
    
    /** Finds the largest index of a match between two strings.
     * @param   str1    First string as base of comparison.
     * @param   str2    Second string to compare.
     * @return  -1 if there are no matching characters; otherwise the index at which the last
     *             matching character occurred in both strings.
     */
    public int matchIndexOf(String str1, String str2) {
        int matchIdx = -1;
        for (int i = 0; ((i < str1.length()) && (i < str2.length())); i++) {
            if (str1.charAt(i) == str2.charAt(i)) {
                matchIdx = i;
            } else {
                break;
            }
        }
        return matchIdx;
    }
    
    /** Tester.
     * <pre>
     *  java -cp <appropriate classpath> com.sun.wsdl.model.uri.impl.URIImpl relativize
     *      C:/Document/Settings/Guest/1234567890/acme/wsdl/order/soapOrder.wsdl
     *      C:/Document/Settings/Guest/1234567890/acme/xsd/order/enterprisetypes.xsd
     * </pre>
     *
     * @param   args    The argument array containing:
     *                  <ol start="0">
     *                  <li>"relativize"</li>
     *                  <li>The base absolute resource path</li>
     *                  <li>The target absolute resource path</li>
     *                  </ol>
     * @throws  Throwable   When good code go bad.
     */
    public static void main(String[] args) {
        if ("relativize".equalsIgnoreCase(args[0]) && (args.length >= 3)) {
            URI uriBase = new URIImpl(new File(args[1]).getAbsolutePath());
            URI relTarg = uriBase.relativize(new File(args[2]).getAbsolutePath());
            System.out.println("Relative path is: " + relTarg);
            System.out.println("Roundtrip: " + uriBase.resolve(relTarg));
        }
    }
}
