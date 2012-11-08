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
 * @(#)Option.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcsre;

import java.util.*;
import java.io.*;

/**
 * Class to implement normal Unix-style getopt().
 * Supports single-letter options, optional arguments, "--" as option
 * list terminator, and multiple options in a single argument.
 */
public class Option
{
    private String[] args;
    private String valid;
    private int optpos;
    private int optind;
    private boolean end;
    public boolean debug = false;

    /**
     * Constructs from a set of input arguments, plus a string describing
     * the legal one-letter options.  In the "valid" string, each legal
     * optional letter must be present (in any order), optionally followed
     * by a ":" to indicate that it takes an argument.
     */
    public Option (String[] args, String valid)
    {
	this.args = args;
	this.valid = valid;
	optind = 0;
	optpos = 0;
	end = false;
    }

    /**
     * Gets options index of next argument.
     *
     * @return the index, range 0...N-1
     */
    public int getOptInd ()
    {
	return optind;
    }

    /**
     * Value returned by getOpt() to indicate the end of the options.
     */
    public static final int EOF = -1;

    /**
     * The argument of the current option.
     */
    private String optarg = null;

    /**
     * When getOpt() finds an option that takes an argument, the argument
     * is made available here.  If called for an option without an argument,
     * we complain.
     *
     * @return the option argument
     */
    public String getOptArg ()
    {
       if (optarg == null)
	   throw new RuntimeException("can't get argument for this option");
	return optarg;
    }

    /**
     * Returns next option letter found, or EOF for end.
     * If the option takes an argument, it is return via "optarg".
     * Options that do not take arguments may be followed immediately
     * by another option letter, and those that do take arguments can
     * take either the remainder of the argument string or the next argument.
     * So, assuming a "valid" string "ab:c", the following are equivalent:
     *
     * -a -b hello -c
     * -a -bhello -c
     * -abhello -c
     * -ab hello -c
     *
     * @return a letter value (char) from "valid", or EOF.
     */
    public int getOpt ()
    {
	if (end || optind >= args.length)
	    return EOF;

	// There's something left.
	int len = args[optind].length();
	if (len <= optpos)
	{
	    optind ++;
	    optpos = 0;
	    return getOpt();
	}
	if (optpos == 0)
	{
	    // New argument.
	    if (len == 0)
		return EOF;
	    if (args[optind].charAt(0) == '-')
	    {
		// Argument is option.
		if (len == 1 || (len == 2 && args[optind].charAt(1) == '-'))
		{
		    // Final option: solitary "-" or "--".
		    if (debug)
			System.out.println("[ option end ]");
		    end = true;
		    optind ++;
		    return EOF;
		}
		optpos = 1;
	    }
	    else
	    {
		// Not an option.
		return EOF;
	    }
	}
	// Consume one option character.
	char c = args[optind].charAt(optpos);
	int i = valid.indexOf(c);
	optpos ++;
	if (i < 0)
	{
	    StringBuffer sb = new StringBuffer("unknown option '-" + c
		+ "' (in argument " + optind + "), valid options are:");
	    for (int j = 0; j < valid.length(); j ++)
	    {
	        char v = valid.charAt(j);
	        sb.append((v != ':') ? (" -" + v) : "#");
	    }
	    throw new RuntimeException(sb.toString());
	}
	if (i + 1 < valid.length() && valid.charAt(i + 1) == ':')
	{
	    // Option requires parameter.
	    if (optpos < len)
	    {
		// Parameter is remainder of argument: -xPar
		optarg = args[optind].substring(optpos, len);
		optind ++;
		optpos = 0;
	    }
	    else
	    {
		// Parameter is next argument: -x Par
		if (optind + 1 >= args.length)
		    throw new RuntimeException("missing final parameter");
		optarg = args[optind + 1];
		optind += 2;
		optpos = 0;
	    }
	    if (debug)
		System.out.println("[ option '-" + c + "' <"
		    + optarg + "> ]");
	}
	else
	{
	    // No parameter.
	    optarg = null;
	    if (debug)
		System.out.println("[ option '-" + c + "' ]");
	}
	return c;
    }
}
