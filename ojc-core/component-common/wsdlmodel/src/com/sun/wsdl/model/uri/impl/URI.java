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
 * @(#)URI.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.uri.impl;

import java.net.MalformedURLException;
import java.net.URL;

/**
 * @deprecated  Now separated into URI (abstract interface) and URIImpl (implementation)
 *
 * VERY LIMITED emulation of the Java 1.4 URI class.  When 1.3 is no longer supported,
 * should revert to using standard 1.4 URI class.
 *
 * @author Sun Microsystems
 * @version 
 * @see com.sun.wsdl.model.uri.URI
 * @see com.sun.wsdl.model.uri.impl.URIImpl
 */
public class URI {
    
    /** Holds the URI (converted to forward slashes) */
    private String legalURI;
    
    /** Holds the scheme */
    private String scheme;
    
    /** Holds whether URI is absolute. */
    private boolean absolute = true;
    
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
    public URI() {
    }
    
    /** Constructor for a non opaque URI.
     * @param   scheme      Scheme.
     * @param   absolute    Whether URI is absolute.
     * @param   directory   Directory
     * @param   filename    Filename.
     */
    public URI(String scheme, boolean absolute, String directory, String filename) {
        this.scheme = scheme;
        this.absolute = absolute;
        this.directory = directory;
        this.filename = filename;
        this.opaque = false;
    }
    
    /** Creates a URI.
     * @param   str     URI string.
     * @return  URI corresponding to string.
     */
    public static URI create(String str) {
        URI ru = new URI();
        ru.legalURI = str.replace('\\', '/');
        int colon = ru.legalURI.indexOf(':');
        ru.scheme = (colon != -1 ? ru.legalURI.substring(0, colon) : "file");
        if (isDOSDrive(ru.scheme)) {
            ru.scheme = "file";
            colon = -1;
        }
        
        ru.opaque = !("file".equals(ru.scheme) || "http".equals(ru.scheme));
        if (!ru.opaque) {
            int lastSlash = ru.legalURI.lastIndexOf('/');
            if (lastSlash != -1) {
                ru.directory = ru.legalURI.substring(colon + 1, lastSlash);
                if (ru.legalURI.length() > (lastSlash + 1)) {
                    ru.filename = ru.legalURI.substring(lastSlash + 1);
                }
            } else {
                ru.filename = ru.legalURI.substring(colon + 1);
            }
            ru.absolute = ((ru.directory != null) && ru.directory.startsWith("/"));
        }
        return ru;
    }
    
    /** Tests if possible scheme is indeed a DOS drive letter.
     * @param   scheme  Scheme to check.
     * @return  <code>true</code> if scheme is a DOS drive.
     */
    private static boolean isDOSDrive(String scheme) {
        return ((scheme.length() == 1)
            && ((Character.getType(scheme.charAt(0)) == Character.UPPERCASE_LETTER)
                || (Character.getType(scheme.charAt(0)) == Character.LOWERCASE_LETTER)));
    }
    
    /** Gets the scheme portion of the URI.
     * @return  URI schem.
     */
    public String getScheme() {
        return scheme;
    }
    
    /** Gets the directory portion of the URI.
     * @return  Directory.
     */
    public String getDirectory() {
        return (directory != null ? directory : ".");
    }
    
    /** Gets the filename portion of the URI.
     * @return  Filename.
     */
    public String getFilename() {
        return (filename != null ? filename : "");
    }
    
    /** Tests if the URI is opaque.
     * @return  <code>true</code> if URI is opaque.
     */
    public boolean isOpaque() {
        return opaque;
    }
    
    /** Tests if the URI is absolute.
     * @return  <code>true</code> if URI is absolute.
     */
    public boolean isAbsolute() {
        return absolute;
    }
    
    /** Resolves another URI relative to this.
     * @param   str     URI string to resolve.
     * @return  Resolved URI
     */
    public URI resolve(String str) {
        URI ru = create(str);
        if (ru.isOpaque() || ru.isAbsolute()) {
            return ru;
        }
        String ndir = httpFixRootDirectory(normalizePath(getDirectory() + "/" + ru.getDirectory()));
        return new URI(getScheme(), isAbsolute(), ndir, ru.getFilename());
    }
    
    /** Normalizes this URI (gets rid of unnecessary ../ directories).
     * @return  Normalized URI
     */
    public URI normalize() {
        if (isOpaque()) {
            return this;
        }
        String ndir = httpFixRootDirectory(normalizePath(getDirectory()));
        return new URI(getScheme(), isAbsolute(), ndir, getFilename());
    }
    
    /** Forms the URL representation of this URI.
     * @return  URL representation.
     * @throws  MalformedURLException   If cannot be converted to URL.
     */
    public URL toURL() throws MalformedURLException {
        return new URL(toString());
    }
    
    // Following until (***) is courtesy Sun JDK 1.4 java.net.URI which is not part of JDK 1.3
    
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
    private static int needsNormalization(String path) {
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
    private static void split(char[] path, int[] segs) {
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
    private static int join(char[] path, int[] segs) {
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
    private static void removeDots(char[] path, int[] segs) {
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
    private static void maybeAddLeadingDot(char[] path, int[] segs) {
        
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
    
    /** Gets string representation.
     * @return  String representation.
     */
    public String toString() {
        return (opaque ? legalURI : (getScheme() + ":" + dirConcatFile(getDirectory(), getFilename())));
    }
    
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
}
