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
 * @(#)SSC_FileSet.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs.ssc;
import java.util.*;

/**
 * Class to represent the closure of an SSC-based XSC file and its references.
 * Part of the e*Gate Java collaboration service tools.
 */
public class SSC_FileSet
{
    // Map normalized XSC pathnames to SSC_File instances.
    private Hashtable sscFiles;
    private SSC_File main = null;

    /**
     * Creates as an empty set.
     */
    public SSC_FileSet ()
    {
	sscFiles = new Hashtable();
    }

    /**
     * Finds an XSC external template file by its normalized name,
     * i.e. the filepath relative to the XSC root.
     * As a special case, return the main file when the path is null.
     *
     * @param path  the normalized path of the template
     * @return the template file, or null if not found
     */
    public SSC_File resolveGlobalTemplate (String path)
    {
	if (path == null) { return main; }
	return (SSC_File) sscFiles.get(path);
    }

    /**
     * Registers an SSC_File instance with its XSC filename.
     * If "main" is set, this is the top-level file; there can only be one.
     *
     * @param file  SSC-based XSC file to register
     * @param main  flag: is this the (only) top-level file?
     */
    public void defineGlobalTemplate (SSC_File file, boolean main)
    {
	sscFiles.put(file.getXscFileName(), file);
	if (main) { this.main = file; }
    }

    /**
     * Registers an SSC_File instance for a templatewith its XSC filename.
     *
     * @param file  SSC-based XSC template file to register
     */
    public void defineGlobalTemplate (SSC_File file)
    {
	defineGlobalTemplate(file, main == null);
    }

    /**
     * Provides a way to loop over all the files in the set.
     *
     * @return an iterator for the whole set.
     */
    public Iterator getFiles ()
    {
	return sscFiles.values().iterator();
    }
}
