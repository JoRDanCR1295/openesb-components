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
 * @(#)ClassTree.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs.ssc;

import java.io.PrintStream;

import com.sun.stc.jcsre.JCSProperties;
import com.sun.stc.jcsre.Option;

/**
 * Class used to analyze the tree structure spanned by the packages
 * and classes of the code generated for ETDs.
 * <p>
 * The package names, class file basenames, and (inner) class names
 * form a tree-shaped hierarchy of name spaces.  The fully qualified
 * name of a class corresponds to a path through this tree, from its
 * root to one of its leaves.  The path consists of four segments,
 * in this order: package name, file basename, ancestor inner classes,
 * and inner class name.  In a fully qualified name, any of these
 * four segments may be empty except the file basename.  Generically
 * we call nodes in the tree either package parts or class names, so
 * a path is a series of package parts followed by a set of class
 * names.  In Java, any node in the tree can only be either a package
 * part or a class name, not both: if you have a class "a.b" where
 * "a" is the package and "b" is the file basename, then there can
 * be no package "a.b", so any path "a.b.c" must be the name of an
 * inner class "c" within the top-level class "b".
 * <p>
 * The ClassTree node class is used to construct an overview of the
 * tree, to query nodes for their assumed type (package part,
 * basename, inner class, unknown), and to register which other
 * items depend on node types, so we can resolve conflicts.
 * <p>
 * The syntax for any path, expressed as the succession of types (see
 * PathNode for an explanation) is:
 *
 *   THEROOT {PACKAGE} [(UPCLASS | {UNKNOWN} ISCLASS) {INCLASS}]
 *   {BLOCKED}
 */
public class ClassTree
{
    public boolean debug =
	JCSProperties.getFlag("ClassTree.debug", false);

    /**
     * Interface to make uses of a PathNode accessible for error message info.
     */
    public static interface PathUser
    {
	/**
	 * Gets the user information as a string, suitable for locating
	 * the use occurrence in the input.
	 * For XSC analysis, the information should include the XSC file
	 * name and the path of original names (not the "javaName" values)
	 * to the node.
	 *
	 * @param node  the node on which we want information
	 */
	public String getOrigin (PathNode node);
    }

    /**
     * Exception to throw for errors reportable to the user for conflicts.
     */
    public static class TreeException extends RuntimeException
    {
	public TreeException (String message) { super(message); }
    }

    /**
     * Class to represent a single node in the class tree.
     * Each class node has a name, a "user" (a back reference to whoever
     * first defined this node to exist), an optional parent, and a list
     * (possibly empty) of children.  Children are linked by their "next"
     * field to their siblings.  Only the root node has no parent.
     */
    public class PathNode
    {
	/**
	 * This node is the root of the tree.  It has no parent.
	 */
	public static final byte THEROOT = 0; // not sure yet

	/**
	 * Nothing known about this node yet; could be package part
	 * or class.  When we register A.B.C as a fully qualified
	 * class name without specifying which is the file basename,
	 * then C is ISCLASS, and A and B are UNKNOWN.
	 * Its parent may be THEROOT, PACKAGE, or UNKNOWN; children
	 * can be UNKNOWN, ISCLASS, or BLOCKED.
	 */
	public static final byte UNKNOWN = 1; // not sure yet

	/**
	 * Node is a package part, hence cannot also be used as a
	 * file basename or inner class name.  Its parent will be
	 * THEROOT or PACKAGE, its children either UNKNOWN,
	 * PACKAGE, or UPCLASS.
	 */
	public static final byte PACKAGE = 2; // package part

	/**
	 * Node is a class name, either file basename or inner class.
	 * Its parent will be UNKNOWN, its children will be INCLASS or BLOCKED.
	 */
	public static final byte ISCLASS = 3; // class name

	/**
	 * Node is file basename, i.e.&nbsp;an outermost class.
	 * Its parent will be THEROOT or PACKAGE, and its children
	 * (if any) will be INCLASS or BLOCKED.
	 */
	public static final byte UPCLASS = 4; // file basename

	/**
	 * Node is in use as an inner class.  Parent may be UPCLASS or
	 * INCLASS; children may be INCLASS or BLOCKED.
	 */
	public static final byte INCLASS = 5; // inner class name

	/**
	 * Node is something predefined; used to block off some part of
	 * the name space.  Parent may be anything, children can only
	 * be BLOCKED.
	 */
	public static final byte BLOCKED = 6; // reserved space

	/**
	 * Not a valid node type; used for error signalling.
	 */
	public static final byte ILLEGAL = 7;

	private byte type; // kind of node; one of UNKNOWN etc.
	private String name; // node name
	private PathUser user; // who uses this node?
	private PathNode parent; // ancestor node; null = root
	private PathNode kids; // link to first child
	private PathNode next; // link to next sibling
	private boolean used; // flag: node claimed uniquely?

	/**
	 * Constructs from a simple name and a parent.
	 * The user reference registered here may be replaced later,
	 * if the type information is weak (UNKNOWN, ISCLASS) and
	 * replaced by a more definite type later (PACKAGE, UPCLASS, INCLASS).
	 *
	 * The new node is automatically inserted in its parent's list of
	 * children.  Children are not inserted in any particular order.
	 * This method does not check for duplicate names.
	 *
	 * @param user  reference to node definer
	 * @param name  the node name
	 * @param below  parent (null for root)
	 * @param type  the node type (PACKAGE, UPCLASS...)
	 * @param used  flag: node uniquely claimed?
	 */
	public PathNode (PathUser user, String name, PathNode below, byte type,
	    boolean used)
	{
	    this.user = user;
	    this.name = name;
	    this.type = type;
	    this.used = used;
	    this.parent = below;
	    this.kids = null;
	    this.next = null;
	    if (below != null)
	    {
		// Insert into parent's list.
		this.next = below.getKids();
		below.setKids(this);
	    }
	}

	// Property access.
	public byte getType () { return type; }
	public String getName () { return name; }
	public boolean getUsed () { return used; }
	public PathNode getParent () { return parent; }
	public PathNode getNext () { return next; }
	public PathUser getUser () { return user; }
	public void setUsed (boolean used) { this.used = used; }
	public void setKids (PathNode kids) { this.kids = kids; }

	/**
	 * Update the type of the existing node.
	 * Used when a more definite type is established.
	 *
	 * @param type  the new type (PACKAGE, UPCLASS, ...)
	 */
	public void setType (byte type)
	{
	    if (debug)
		System.out.println(" - change [" + getPath()
		    + "] from " + typeName(this.type) + " -> "
		    + typeName(type));
	    this.type = type;
	}

	/**
	 * Gets the link to the first child, if any.
	 * You can iterate though the remaining children using getNext().
	 *
	 * @return the first child, if any
	 */
	public PathNode getKids () { return kids; }

	/**
	 * Change the user reference.  This update should normally be done
	 * when changing the node type to something more definite, e.g. from
	 * UNKNOWN to UPCLASS.  The user reference should reflect who most
	 * definitely pinned down the type of the node, so it can be reported
	 * in conflict error messages.
	 *
	 * @param user  the new user reference
	 */
	public void setUser (PathUser user) { this.user = user; }

	/**
	 * Given a node, make the path expression as a series of
	 * dot-separated names.  Not very effificent, but only used
	 * for error messages anyway.
	 *
	 * @return the string
	 */
	public String getPath ()
	{
	    if (type == THEROOT) { return null; }
	    String prefix = parent.getPath();
	    return (prefix == null) ? name : (prefix + '.' + name);
	}
    }

    /**
     * The root node of the tree.  Must have type THEROOT.
     */
    private PathNode root;

    /**
     * The number of nodes in the tree.  When find() returns a node,
     * you can use this to detect whether the node was newly created,
     * by checking the count before and after.
     */
    private int size = 0;

    // Access.
    public int getSize () { return size; }

    /**
     * Constructs with a root, with no children.
     */
    public ClassTree ()
    {
	this.root = new PathNode(null, "[root]", null, PathNode.THEROOT, false);
	this.size = 1;
    }

    /**
     * Gets the root node of the tree.  This does not correspond to any
     * class or package, and will always have type THEROOT.  Instead of
     * calling this method, you could also use find(null, null, null,
     * PathNode.THEROOT, false, false)...
     *
     * @return the tree root node
     */
    public PathNode getRoot () { return root; }

    /**
     * Makes printable string for character value.
     *
     * @param u  the Unicode character
     * @return a string
     */
    public static String charShow (char u)
    {
	return (u != '\'' && 0x20 <= u && u <= 0x7E)
	    ? ("'" + u + "'")
	    : ("U+" + Integer.toHexString((int) u + 0x10000).substring(1));
    }

    // Flag: show user into in print dump?
    public boolean showUser = false;

    /**
     * Auxiliary for printing the tree; recursive with prefix string.
     *
     * @param out  the output stream
     * @param pf0  the prefix string for the main line
     * @param pf1  the prefix string for all following lines
     * @param node  the node to display
     * @return the node count
     */
    public int print (PrintStream out, String pf0, String pf1, PathNode node)
    {
	int count = 1;
	char kind = '?';
	switch (node.getType())
	{
	case PathNode.THEROOT: kind = 'R'; break;
	case PathNode.PACKAGE: kind = 'P'; break;
	case PathNode.UNKNOWN: kind = 'U'; break;
	case PathNode.UPCLASS: kind = 'T'; break;
	case PathNode.ISCLASS: kind = 'C'; break;
	case PathNode.INCLASS: kind = 'I'; break;
	case PathNode.BLOCKED: kind = 'X'; break;
	}
	PathUser user = node.getUser();
	out.println(pf0 + kind + ' ' + node.getName()
	    + (node.getUsed() ? "*" : "")
	    + ((showUser && user != null)
		? (" (" + user.getOrigin(node) + ")")
		: ""));
	PathNode q;
	for (PathNode p = node.getKids(); p != null; p = q)
	{
	    q = p.getNext();
	    out.println(pf1 + "|");
	    count += print(out, pf1 + "+-",
		pf1 + (q == null ? "  " : "| "), p);
	}
	return count;
    }

    /**
     * Dumps a representation of the node tree to the given output.
     *
     * @param out  the output stream
     */
    public void print (PrintStream out)
    {
	int count = 0;
	if (root != null) { count = print(out, "", "", root); }
	out.println();
	out.println("Total nodes: " + count);
    }

    /**
     * Try to infer the type of a new intermediate node from what we
     * know about the final node's type and the given ancestor higher
     * up in the tree.  If we cannot find a legal type for the intermediate
     * node, return ILLEGAL.
     */
    private byte between (PathNode at, byte ultimate)
    {
	switch (at.getType())
	{
	case PathNode.THEROOT:
	case PathNode.PACKAGE:
	case PathNode.UNKNOWN:
	    // We have no clue.
	    if (ultimate != at.THEROOT)
		return at.UNKNOWN;
	    break;

	case PathNode.BLOCKED:
	    // We're below a blocked name space, can only block more.
	    if (ultimate == at.BLOCKED)
		return at.BLOCKED;
	    break;

	case PathNode.UPCLASS:
	case PathNode.ISCLASS:
	case PathNode.INCLASS:
	    // We're somewhere below a file basename, must be class.
	    switch (ultimate)
	    {
	    case PathNode.BLOCKED:
	    case PathNode.ISCLASS:
	    case PathNode.INCLASS:
		return at.INCLASS;
	    }
	    break;
	}
	return at.ILLEGAL;
    }

    /**
     * Update all children of given node to be an inner class space.
     * This is done when the given node used to be UNKNOWN (i.e. could
     * be package or class), so paths from here would be of the form
     * {UNKNOWN} ISCLASS.  We need to change all the UNKNOWN to INCLASS
     * (unclaimed inner class), and all ISCLASS to INCLASS (definite inner
     * class).  We don't update the user, because ISCLASS means it's
     * got a claim, and INCLASS is not much more definite than UNKNOWN.
     *
     * @param node  the root of the subtree to confirm
     */
    private void confirmClass (PathNode node)
    {
	for (PathNode kid = node.getKids(); kid != null; kid = kid.getNext())
	{
	    if (kid.getType() == kid.ISCLASS)
	    {
		// End of descent.
		kid.setType(kid.INCLASS);
	    }
	    else if (kid.getType() == kid.UNKNOWN)
	    {
		// Do the subtree.
		kid.setType(kid.INCLASS);
		confirmClass(kid);
	    }
	}
    }

    /**
     * Given a starting point in the tree, finds the node that corresponds
     * to the given subpath.  The subpath is given as a string consisting
     * of a series of names separated by dots.  If the subpath is not found,
     * create it if "make" is set.  Return the node, or null if neither
     * found nor created.
     *
     * @param user  reference to caller, for created nodes
     * @param from  the starting node (null = path starts with root)
     * @param path  the dot-separated node name sequence
     * @param type  the kind of node at the end
     * @param used  flag: claim node uniquely for caller?
     * @param make  flag: create new if not found?
     * @return the node found or created, or null
     *
     * @throws IllegalArgumentException for malformed path
     * @throws TreeException for type conflicts
     */
    public PathNode find (PathUser user, PathNode from, String path,
	byte type, boolean used, boolean make)
	throws IllegalArgumentException
    {
	if (from == null) { from = this.root; }
	PathNode at = from;
	String rest = path;
	String part = null;
	String act = (make ? "define " : "accept ");
	for (int dots = 0; rest != null; dots ++)
	{
	    // Split off first component of remainder of path.
	    int split = rest.indexOf('.');
	    if (split >= 0)
	    {
		// We have multiple parts.
		part = rest.substring(0, split);
		rest = rest.substring(split + 1);
	    }
	    else
	    {
		// This is the last part.
		part = rest;
		rest = null;
	    }
	    boolean last = (rest == null);
	    String plow = part.toLowerCase();
	    if (plow.equals(part))
		plow = null;
	    int plen = part.length();
	    if (plen == 0)
	    {
		// Path components must be at least 1 char.
		throw new IllegalArgumentException(
		    "invalid node path [" + path
		    + "], empty part after " + dots + " dots");
	    }
	    // Check name for illegal characters.
	    for (int i = 0; i < plen; i ++)
	    {
		char u = part.charAt(i);
		if (i == 0
		    ? !Character.isJavaIdentifierStart(u)
		    : !Character.isJavaIdentifierPart(u))
		{
		    // Not a proper Java name.
		    throw new IllegalArgumentException(
			"invalid node path [" + path
			+ "], illegal char " + charShow(u)
			+ " at offset " + i + " in part [" + part + "]");
		}
	    }
	    // Search current parent for existing child with this name.
	    PathNode here = null;
	    for (PathNode sub = at.getKids(); sub != null; sub = sub.getNext())
	    {
		String name = sub.getName();
		if (name.equals(part))
		    { here = sub; break; }

		// Check for potential clash on Windows.
		if (plow != null && name.toLowerCase().equals(plow))
		    throw new TreeException("name [" + part + "] in path ["
			+ path + "] for " + user.getOrigin(null)
			+ " has potential case-insensitive clash with ["
			+ name + "] in [" + sub.getPath() + "] "
			+ (sub.getUsed() ? "defined" : "implied")
			+ " by "
			+ (sub.getUser() == null
			    ? "system"
			    : sub.getUser().getOrigin(sub)));
	    }
	    if (here == null)
	    {
		// Not found; create or fail.
		if (! make) { return null; }
		byte kind = (last ? type : between(at, type));
		if (debug)
		    System.out.println(" - part <" + part + "> not found, make "
			+ typeName(kind));
		if (kind == at.ILLEGAL)
		{
		    // Cannot make intermediate node; something wrong.
		    String upto = at.getPath();
		    throw new TreeException(
			"cannot " + act + typeName(kind)
			+ " [" + upto + '.' + part + "] for "
			+ user.getOrigin(null)
			+ (last ? ""
			    : (", while making " + typeName(type) + " [" +
				path + "]"))
			+ ", because" + upto
			+ " was defined as " + typeName(at.getType())
			+ " by " + at.getUser().getOrigin(at));
		}
		here = new PathNode(user, part, at, kind,
		    (last ? used : false));
		size ++;
	    }
	    else if (! last)
	    {
		// Found node, and not at end of path yet.
		if (debug)
		    System.out.println(" - prior part <" + part + "> found as "
			+ typeName(here.getType()));
		boolean failed = false;
		switch (here.getType())
		{
		case PathNode.UPCLASS:
		case PathNode.ISCLASS:
		case PathNode.INCLASS:
		    failed = (type == at.PACKAGE);
		    break;

		case PathNode.BLOCKED:
		    failed = (type != at.BLOCKED);
		    break;
		}
		if (failed)
		{
		    // Intermediate found conflicts with final node.
		    PathUser prior = here.getUser();
		    throw new TreeException(
			"cannot " + act + typeName(type)
			+ " [" + path + "] for "
			+ user.getOrigin(null)
			+ ", because [" + here.getPath()
			+ "] was defined as " + typeName(here.getType())
			+ (prior == null
			    ? ""
			    : (" by " + prior.getOrigin(here))));
		}
	    }
	    else
	    {
		// Found node, this must be the final one.
		if (debug)
		    System.out.println(" - final part <" + part + "> found as "
			+ typeName(here.getType()));
		boolean failed = (here.getType() != type);
		boolean change = false;
		switch (here.getType())
		{
		case PathNode.PACKAGE:
		    if (type == at.UNKNOWN) { failed = false; }
		    break;

		case PathNode.UNKNOWN:
		    switch (type)
		    {
		    case PathNode.PACKAGE:
		    case PathNode.UPCLASS:
		    case PathNode.ISCLASS:
		    case PathNode.INCLASS:
			change = true;
			failed = false;
		    }
		    break;

		case PathNode.UPCLASS:
		    switch (type)
		    {
		    case PathNode.UNKNOWN:
		    case PathNode.ISCLASS:
			failed = false;
		    }
		    break;

		case PathNode.ISCLASS:
		    switch (type)
		    {
		    case PathNode.UPCLASS:
		    case PathNode.INCLASS:
			change = true;
		    case PathNode.UNKNOWN:
			failed = false;
		    }
		    break;

		case PathNode.INCLASS:
		    switch (type)
		    {
		    case PathNode.UNKNOWN:
		    case PathNode.ISCLASS:
			failed = false;
		    }
		    break;
		}
		failed |= (used && here.getUsed());
		if (failed)
		{
		    // Final node found conflicts with requested type.
		    PathUser prior = here.getUser();
		    throw new TreeException(
			"cannot " + act + typeName(type)
			+ " [" + here.getPath() + "] for "
			+ user.getOrigin(null)
			+ ", because it was "
			+ (here.getUsed() ? "defined" : "implied")
			+ " as "
			+ typeName(here.getType())
			+ (prior == null
			    ? ""
			    : (" by " + prior.getOrigin(here))));
		}
		if (change)
		{
		    // Refine the type.
		    here.setUser(user);
		    here.setType(type);
		}
		if (used && ! here.getUsed())
		{
		    here.setUser(user);
		    here.setUsed(true);
		}
	    }

	    // Post-process ancestors, because we know more now.
	    switch (here.getType())
	    {
	    case PathNode.UPCLASS:
		confirmClass(here);
	    case PathNode.PACKAGE:
		/* Added/found a package or a file basename, everything above
		 * it must be a package.  It we hit a package on the way
		 * up, assume everything above it is already okay.
		 */
		for (PathNode up = at; up.getType() == up.UNKNOWN;
		    up = up.getParent())
		{
		    // Make into a package.
		    up.setType(up.PACKAGE);
		    up.setUser(user);
		    for (PathNode kid = up.getKids(); kid != null;
			kid = kid.getNext())
		    {
			/* Immediately below new PACKAGE node, turn any
			 * ISCLASS (inner/outer class) into a file basename.
			 */
			if (kid.getType() == kid.ISCLASS)
			    kid.setType(kid.UPCLASS);
		    }
		}
		break;

	    case PathNode.ISCLASS:
		/* Added/found a tentative class (could be inner or top-level).
		 * turn into INCLASS (definite inner) or UPCLASS (top) if
		 * we can determine which it is.
		 */
		switch (at.getType())
		{
		case PathNode.THEROOT:
		case PathNode.PACKAGE:
		    // Below package, must be outermost.
		    here.setType(here.UPCLASS);
		    here.setUser(user);
		    break;

		case PathNode.UPCLASS:
		case PathNode.ISCLASS:
		case PathNode.INCLASS:
		    // Below class, must be inner class.
		    here.setType(here.INCLASS);
		    here.setUser(user);
		    break;
		}
		confirmClass(here);
		break;

	    case PathNode.INCLASS:
		// Just get rid of UNKNOWN below this.
		confirmClass(here);
		break;
	    }
	    at = here;
	}
	return at;
    }

    /**
     * Convenience wrapper for find(): find (or make) package path.
     * If the path is empty or null, return the root.
     *
     * @param user  reference to caller, for created nodes
     * @param path  the dot-separated node name sequence
     * @return the node found or created
     * @throws IllegalArgumentException for malformed path
     * @throws TreeException for type conflicts
     */
    public PathNode findPackage (PathUser user, String path)
	throws IllegalArgumentException
    {
	return (path == null || path.equals(""))
	    ? root
	    : find(user, null, path, PathNode.PACKAGE, false, true);
    }

    /**
     * Convenience wrapper for find(): block given name below root.
     * If the class already existed, complain.
     *
     * @param path  the dot-separated node name sequence
     * @throws IllegalArgumentException for malformed path
     * @throws TreeException for type conflicts, or if class already there
     */
    public void block (String path)
	throws IllegalArgumentException, TreeException
    {
	find(null, null, path, PathNode.BLOCKED, false, true);
    }

    /**
     * Convenience wrapper for find(): make new class node.
     * If the class already existed, complain.
     *
     * @param user  reference to caller, for created nodes
     * @param below  the parent, or null for root
     * @param path  the dot-separated node name sequence
     * @throws IllegalArgumentException for malformed path
     * @throws TreeException for type conflicts, or if class already there
     */
    public void makeNewClass (PathUser user, PathNode below, String path)
	throws IllegalArgumentException, TreeException
    {
	PathNode pn = find(user, below, path, PathNode.ISCLASS, true, true);
	if (debug)
	    System.out.println(" - added [" + path + "] as "
		+ typeName(pn.getType()));
    }

    /**
     * Provide a printable name for a node type.
     *
     * @return a string
     */
    public static String typeName (byte type)
    {
	switch (type)
	{
	case PathNode.THEROOT: return "root";
	case PathNode.PACKAGE: return "package";
	case PathNode.UNKNOWN: return "package-or-class";
	case PathNode.UPCLASS: return "outer class";
	case PathNode.ISCLASS: return "class";
	case PathNode.INCLASS: return "inner class";
	case PathNode.BLOCKED: return "reserved space";
	case PathNode.ILLEGAL: return "illegal";
	}
	throw new RuntimeException("impossible node type (#" + type + ")");
    }

    // Used for testing.
    private static class FakeUser implements PathUser
    {
	private String text;
	public FakeUser (String text) { this.text = text; }
	public String getOrigin (PathNode node) { return text; }
    }

    /**
     * Tests the class by processing all arguments as paths to insert,
     * and then printing out the resulting tree.  Each path must be
     * given as an option with a letter to indicate the type of the leaf:
     *
     * -R = THEROOT, -P = PACKAGE, -U = UNKNOWN, -T = UPCLASS,
     * -C = ISCLASS, -I = INCLASS, -X = BLOCKED.
     *
     * @param args  the paths
     */
    public static void main (String[] args)
    {
	try
	{
	    ClassTree tree = new ClassTree();
	    boolean make = true, used = false;
	    int c = 0, test = 0;

	    Option opt = new Option(args, "cdfouC:I:P:R:T:U:X:");
	    while ((c = opt.getOpt()) != Option.EOF)
	    {
		byte type = PathNode.ILLEGAL;
		switch (c)
		{
		case 'c':
		    // Option -c: create fresh node.
		    make = true;
		    used = false;
		    break;
		case 'd':
		    // Option -d: switch on debugging.
		    tree.debug = true;
		    break;
		case 'f':
		    // Option -f: find only, no new nodes.
		    make = false;
		    used = false;
		    break;
		case 'o':
		    // Option -o: show origin info in print.
		    tree.showUser = true;
		    break;
		case 'u':
		    // Option -u: create, claim new nodes uniquely.
		    make = true;
		    used = true;
		    break;

		case 'C': type = PathNode.ISCLASS; break;
		case 'I': type = PathNode.INCLASS; break;
		case 'P': type = PathNode.PACKAGE; break;
		case 'R': type = PathNode.THEROOT; break;
		case 'T': type = PathNode.UPCLASS; break;
		case 'U': type = PathNode.UNKNOWN; break;
		case 'X': type = PathNode.BLOCKED; break;

		default:
		    // Huh?
		    throw new RuntimeException("Unknown option: -" + c);
		}
		if (type != PathNode.ILLEGAL)
		{
		    String testName = "test-" + (test ++);
		    FakeUser user = new FakeUser(testName);
		    String path = opt.getOptArg();
		    System.out.println((make ? "Insert " : "Fathom ")
			+ testName + " [" + path + "] as " + typeName(type));
		    int prior = tree.getSize();
		    try
		    {
			PathNode node = tree.find(user, null, path, type,
			    used, make);
			if (node == null)
			    System.out.println(" -> node not found");
			if (prior != tree.getSize())
			    System.out.println(" -> new node created");
		    }
		    catch (TreeException t)
		    {
			System.out.println("Failed check, message: "
			    + t.getMessage());
		    }
		}
	    }
	    int oind = opt.getOptInd();
	    for (int i = oind; i < args.length; i ++)
		System.out.println("Ignored argument [" + args[i] + "]");
	    tree.print(System.out);
	}
	catch (Exception exc)
	{
	    exc.printStackTrace();
	}
    }
}
