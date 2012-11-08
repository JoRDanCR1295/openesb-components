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
 * @(#)CocoDataModel.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.model;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Vector;
import java.util.ListIterator;
import java.util.HashMap;
import java.util.Map;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.sun.encoder.coco.runtime.messages.ErrorManager;
import com.sun.encoder.coco.runtime.messages.Message;
import com.sun.encoder.coco.runtime.messages.MessageCatalog;

/**
 * The data model for information parsed from Cobol Copybook input.
 *
 * @author  Noel Ang
 *
 * @version $Revision: 1.2 $
 *
 * @see     com.sun.encoder.coco.model.CocoParser
 */
public class CocoDataModel {

    private CocoDescriptionEntry mRootEntry;
    private List<CocoDescriptionEntry> mEntries;
    private boolean mImplied01;
    private long mFillerCount;
    private long mBlankCount;
    // FQN collision table
    private Map<String, Object> mNames;
    // base name collision table
    private Map<String, Vector> mBaseNames;
    private Map<String, Integer> mHlqs;
    private List mUnresolvedOccurs;
    
    private final ErrorManager mErrorMgr =
            ErrorManager.getManager("OpenESB.encoder.CobolCopybook."
                                    + getClass().getName());
    
    /**
     * Default constructor. Creates an empty data model.
     */
    public CocoDataModel() {
        mEntries =
            Collections.synchronizedList(
                    new ArrayList<CocoDescriptionEntry>());
        mRootEntry = new CocoDescriptionEntry("FILLER", 1);
        mRootEntry.setFQN(mRootEntry.getName());
        mImplied01 = true;
        mFillerCount = 0;
        mBlankCount = 0;
        mNames = new HashMap<String, Object>();
        mBaseNames = new HashMap<String, Vector>();
        mHlqs = new HashMap<String, Integer>();
        
        mEntries.add(mRootEntry);
    }
    
    /**
     * Add a description entry to the model. The entry is validated prior to
     * being added, for such things such as names, level numbers,
     * (TODO) mutually exclusive clauses, etc.
     *
     * @param entry The description entry to add
     *
     * @throws java.lang.IllegalArgumentException if the entry cannot be
     *          validated for this model
     */
    public void addEntry(CocoDescriptionEntry entry)
        throws IllegalArgumentException {
    
        validateName(entry);
    
    
        /* ignore special levels */
        int level = entry.getLevel();
        if (level < 1 || level == 66 || level == 77 || level == 88) {
            return;
        }
    
        mEntries.add(entry);
    
        /*
         * redefined entries already keep references to redefinitions, so
         * ignore starting redefs
         */
        if (entry.isRedefinition()) {
        	// need to register base name and FQN here
        	// also - redef should not conflict with any same level entry 
        	// within the same parent entry;
        	CocoDescriptionEntry redefTarget = entry.getRedefinedTarget();
        	if ( redefTarget != null ) {
        		this.registerBaseName(entry);
        		CocoDescriptionEntry parent = redefTarget.getParent();
        		String fqn = null;
        		if ( parent != null )
        			fqn = parent.getFQN() + ":" + entry.getName();
        		else
        			fqn = entry.getName();
    			entry.setFQN(fqn);
        		if ( this.mNames.containsKey(fqn) ) {
            		String parentName = "";
            		if ( parent != null )
            			parentName = parent.getInfo();
            		Message msg = MessageCatalog.getMessage("CCCB4228");
                    String text = msg.formatText(new Object[] {
                        entry.getInfo() + "(REDEFINE)",
    					parentName
                    });
                    mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                    throw new IllegalArgumentException(text);
        		}
        		this.registerFQN(entry);
        	}
        	else {
        		// 
        	}
        	return;
        }
    
        if (level == 1) {
            if (mImplied01) {
                mImplied01 = false;
                mEntries.remove(mRootEntry);
                mRootEntry = entry;
                entry.setFQN(entry.getName());
                mNames.put(entry.getFQN(), entry);
                registerBaseName(entry);
            } else {
            	// multi 01 not supported
                Message msg = MessageCatalog.getMessage("CCCB4104");
                String text = msg.toString();
                mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                throw new IllegalArgumentException(text);
            }
        }
        else {
            
            /* once I have begun to add non-level 1 entries, then the
             * current level 1 entry, whether it is an implied 01 or an
             * data input-inserted 01 needs to be locked; no 01 replacement
             * is hereafter possible.
             */
            mImplied01 = false;
            // building the tree - closest scope to the top
            int entryIx = mEntries.size() - 1;
            do {
                CocoDescriptionEntry last =
                    (CocoDescriptionEntry) mEntries.get(entryIx);
                int lastLevel = last.getLevel();
                if (lastLevel < level) {
                	// derive FQN here and check conflict in the current scope
                	last.addChild(entry);
                	// before we add child - check if the parent has a PIC
                	// or its usage is an explicit COMP-1 or COMP-2; 
                	// clause - a group item should not has PIC clause
                	String pic = last.getPicture();
                	if ( pic == null || pic.trim().length() == 0 ) {
                		// this is OK since the parent entry should not 
                		// have a PIC - ONLY elementary item should
                	}
                	else {
                		Message msg = MessageCatalog.getMessage("CCCB4229");
                        String text = msg.formatText(new Object[] {
                            entry.getInfo(),
    						last.getInfo()
                        });
                        mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                        throw new IllegalArgumentException(text);
                	}
                	String fqn = obtainFQN(entry);
                	entry.setFQN(fqn);
                	if ( this.mNames.containsKey(fqn) ) {
                		String parentName = "";
                		CocoDescriptionEntry parent = entry.getParent();
                		if ( parent != null )
                			parentName = parent.getInfo();
                		Message msg = MessageCatalog.getMessage("CCCB4228");
                        String text = msg.formatText(new Object[] {
                            entry.getInfo(),
    						parentName
                        });
                        mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                        throw new IllegalArgumentException(text);
                	}
                	registerFQN(entry);
                	registerBaseName(entry);
                	break;
                } else {
                    entryIx--;
                }
            } while (entryIx >= 0);
        }
    }
    
    /**
     * given a entry in the entry hierarchy, derive its FQN - i.e. 
     * entry's own name qualified by its parents name
     * @param entry_added
     * @return
     */
    private String obtainFQN(CocoDescriptionEntry entry_added) {
    	StringBuffer fqn = new StringBuffer(entry_added.getName());
    	CocoDescriptionEntry parent = entry_added.getParent();
    	if ( parent != null )
    		fqn.insert(0, ":").insert(0, parent.getFQN());
    	return fqn.toString();
    }
    
    /**
     * do all the possible copy book validation here
     * for now, we check if a data item does not have any child 
     * and does not have a PIC clause either 
     */
    public void validate() {
    	if ( mEntries != null ) {
    		int cnt = mEntries.size();
    		CocoDescriptionEntry entry = null;
    		for (int i = 0; i < cnt; i++ ) {
    			entry = (CocoDescriptionEntry)mEntries.get(i);
    			if ( entry.countChildren() == 0 ) {
    				// for leaf item further check if it 
    				// has pic clause - if no - invalid item but
    				// note, COMP-1, COMP-2 does not have PIC but is elementary;
    				// exclude redefine item for this checking
    				if ( !entry.isRedefinition() ) {
    					String pic = entry.getPicture(); 
    					int usage = getRealUsage(entry);
    					// per 93394 further check PIC and USAGE compatibleness
    					if ( usage != CocoDescriptionEntry.UsageType.COMP1
    							&& usage != CocoDescriptionEntry.UsageType.COMP2
    							&& (pic == null || pic.trim().length() == 0) ) {
    		        		Message msg = MessageCatalog.getMessage("CCCB4230");
    		                String text = msg.formatText(new Object[] {
    		                		entry.getInfo()
    		                });
    		                mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
    		                throw new IllegalArgumentException(text);
    					}
    					
    					// COMP1 or COMP2 or PIC
    					// check usage and pic compatibleness - rules (may be
                        // not complete but can block away common seen semantic
                        // bad data description entries)
    
    					// for COMP1 & 2 - this is a non issue
    					// validate only other comp and numeirc entry
    					if ( usage == CocoDescriptionEntry.UsageType.COMP 
    							|| usage == 
                                    CocoDescriptionEntry.UsageType.BINARY 
    							|| usage == 
                                    CocoDescriptionEntry.UsageType.PACDEC 
    							|| usage == 
                                    CocoDescriptionEntry.UsageType.COMP3 
    							|| usage == 
                                    CocoDescriptionEntry.UsageType.COMP5 ) {
    						// should contain only '9', 'S', 's', 'V', 'v',
                            // 'P', 'p' the following regex might be more
                            // relaxed than a valid COBOL numeric data entry
                            // picture - but blocking away those 
    						// that does not match this regex is enough
                            String patternStr =
                                "^[sS]*9*[\\([0-9]+\\)]*[vV]*9*[\\([0-9]+\\)]*[pP]*$";
    						Pattern pattern = 
                                Pattern.compile(patternStr);
    						//Pattern pattern = Pattern.compile("^[9SsVvPp]*$");
    						Matcher match = pattern.matcher(pic); 
    						if ( !match.matches() ) {
    			        		Message msg =
                                    MessageCatalog.getMessage("CCCB4231");
    			                String text = msg.formatText(new Object[] {
    			                		entry.getInfo(),
    									pic,
    									entry.getUsageName(usage)
    			                });
    			                mErrorMgr.log(
                                        ErrorManager.Severity.ERROR,
                                        null, text);
    			                throw new IllegalArgumentException(text);
    						}
    					}
    				}
    			}
    		}
    	}
    }
    
    /**
     * helper - retrieve the real usage of the given entry
     * real usage is the explicit usage of its closest ancester or usage
     * of itself if all the ancesters have implicit usage;
     * 
     * @param entry
     * @return
     */
    private int getRealUsage(CocoDescriptionEntry entry) {
    	int usage = entry.getUsage();
    	CocoDescriptionEntry e = entry;
    	while ( e != null && !e.usageExplicit() )
    		e = e.getParent();
    	if ( e != null )
    		usage = e.getUsage();
    	return usage;
    }
    
    /**
     * Check name of description entry.  Makes sure it is not blank nor a
     * duplicate. If name is "filler" (case ignored) it is made unique by
     * tacking on a number after it.  Same case with "blank".
     * If name contains a high-level qualifier (HLQ) token, do the same.
     *
     * @param  entry The description entry to validate
     *
     * @throws java.lang.IllegalArgumentException if the entry cannot be
     *          validated in this data model
     */
    private void validateName(CocoDescriptionEntry entry)
            throws IllegalArgumentException {
    
        /* name can't be null or blank */
        String name = entry.getName();
        
        if (name == null || "".equals(name.trim())) {
            name = "BLANK";
        }
        
        name = name.toUpperCase();
    
        /* name cannot be FILLER{n} or BLANK{n}, where {n} is some integer */
        if (name.startsWith("FILLER") && !name.equalsIgnoreCase("FILLER")) {
            int at = 5;
            while (++at < name.length()) {
                if (!Character.isDigit(name.charAt(at))) {
                    break;
                }
            }
            if (name.length() == at) {
                Message msg = MessageCatalog.getMessage("CCCB4105");
                String text = msg.formatText(new Object[] {
                    name
                });
                mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                throw new IllegalArgumentException(text);
            }
        }
        if (name.startsWith("BLANK") && !name.equalsIgnoreCase("BLANK")) {
            int at = 4;
            while (++at < name.length()) {
                if (!Character.isDigit(name.charAt(at))) {
                    break;
                }
            }
            if (name.length() == at) {
                Message msg = MessageCatalog.getMessage("CCCB4106");
                String text = msg.formatText(new Object[] {
                    name
                });
                mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                throw new IllegalArgumentException(text);
            }
        }
    
        // FILLER and BLANK is globally unique
        
        /* filler items must be given unique names */
        if (name.equalsIgnoreCase("FILLER")) {
            entry.setName("FILLER" + ++mFillerCount);
            entry.setNameFiller(true);
        /* blank items must be given unique names */
        } else if (name.equalsIgnoreCase("BLANK")) {
            entry.setName("BLANK" + ++mBlankCount);
            entry.setNameBlank(true);
        /* names must be unique */
        } else {
            name = normalizeHlq(name);
            entry.setName(name);
        }
    }
    
    private String normalizeHlq(String name) {
        StringBuffer buffer = new StringBuffer(name.length());
        int hlqStart = name.indexOf(':');
        int start = 0;
        
        /* HLQ names must be normalized */
        while (hlqStart != -1) {
            int hlqEnd = name.indexOf(':', hlqStart + 1);
            if (hlqStart + 1 >= hlqEnd) {
                Message msg = MessageCatalog.getMessage("CCCB4107");
                String text = msg.formatText(new Object[] {
                    name
                });
                mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                throw new IllegalArgumentException(text);
            } else {
                String hlqName =
                    name.substring(hlqStart, ++hlqEnd).toUpperCase();
                if (!mHlqs.containsKey(hlqName)) {
                    mHlqs.put(hlqName, new Integer(mHlqs.size() + 1));
                }
                int hlqNum = ((Integer) mHlqs.get(hlqName)).intValue();
                buffer.append(name.substring(start, hlqStart) + "HLQ" + hlqNum);
                start = hlqEnd;
                hlqStart = name.indexOf(':', start);
            }
        }
        
        buffer.append(name.substring(start));
        return buffer.toString();
    }
    
    /**
     * Retrieve reference to model's record-level entry.
     *
     * @return Reference to the first entry ever added successfully to
     *          the model, or an implicit level-01 (record) entry if no
     *          entry has ever been added.
     */
    public CocoDescriptionEntry getRoot() {
        return mRootEntry;
    }
    
    /**
     * Retrieve reference to a specific entry added to the model.
     *
     * @param  name Name of description entry to get
     *
     * @return The named description entry, or null if that entry does not
     *         exist in the model
     */
    
    // the use of this method is to be reviewed - for now - it is not used
    public CocoDescriptionEntry getEntry(String name, String[] qualifiers) {
    	// this method currently is called from 
    	// parseOccursClause() where the raw data-name is passed in 
    	// to lookup the entry that is used as the target 
    	// of clause DEPENDING ON <data-name>
        if (name == null) {
            return null;
        }
        
        if (mRootEntry.getName().equalsIgnoreCase(name)
            || name.equalsIgnoreCase(mRootEntry.getName())) {
            return mRootEntry;
        }
    
        // the raw name can come in as a simple name or a qualified name
        // before we are sure about the syntax of qualified name
        // let's deal with simple name first
    
        // the entry should already have 
        // its name and level
        name = normalizeHlq(name);
        
        CocoDescriptionEntry match = null;
        ListIterator it = mEntries.listIterator();
        //name = (String) mNames.get(name);
        if (name != null) {
            while ((match == null) && it.hasNext()) {
                CocoDescriptionEntry entry = (CocoDescriptionEntry) it.next();
                if (name.equalsIgnoreCase(entry.getName())) {
                    match = entry;
                }
            }
        }
        return match;
    }
    
    /**
     * Find a redefining item's redefined entry.
     *
     * @param redefiner Redefining item
     * @param name      Redefined item's name
     *
     * @return The redefined entry, or <code>null</code> if it could not
     *          be found.
     */
    public CocoDescriptionEntry findRedefineTarget(
            CocoDescriptionEntry redefiner, String name) {
        
        CocoDescriptionEntry target = null;
    
        // name mapping is no longer needed
        //String realName = (String) mNames.get(name.toUpperCase());
    
        // since we do not rename an original name for its global uniqueness
        // the redefined target should be resolved here;
        // the renaming for global uniqueness happens later just before 
        // OTD gen
        
        String realName = name.toUpperCase();
        int ix = mEntries.size() - 1;
    
        // scan the entries from the lowest containing scope
        if (realName != null) {
            while (ix >= 0) {
                CocoDescriptionEntry last =
                    (CocoDescriptionEntry) mEntries.get(ix--);
                int lastLevel = last.getLevel();
                int level = redefiner.getLevel();
                if (lastLevel < level) {
                    break;
                }
                else if (lastLevel == level) {
                    // use original name instead of name, Open-ESB issue 2272
                    if (last.getOriginalName().equalsIgnoreCase(realName)) {
                        target = last;
                        break;
                    }
                }
            }
        }
        return target;
    }
    
    /**
     * used for testing purposes. outputs trace info about model.
     *
     * @param  outStream Outlet to which trace info is sent
     */
    public void toStream(PrintStream outStream) {
        ListIterator it = mEntries.listIterator();
        while (it.hasNext()) {
            CocoDescriptionEntry entry = (CocoDescriptionEntry) it.next();
            if (entry.isComment()) {
                outStream.print("**COMMENT**");
            } else {
                if (entry.isContinuation()) {
                    outStream.print("- ");
                } else {
                    outStream.print("  ");
                }
                outStream.print(entry.getIndicatorValue() + " ");
                outStream.print(entry.getLevel() + " ");
                outStream.print(entry.getName() + " ");
                outStream.print("USAGE " + entry.getUsage() + " ");
                if (entry.isBlankWhenZero()) {
                    outStream.print("BLANK ");
                }
                if (entry.isJustified()) {
                    outStream.print("JUST ");
                }
                String picture = entry.getPicture();
                if (!picture.equals("")) {
                    outStream.print("PIC " + picture + " ");
                }
                if (entry.isRedefinition()) {
                    outStream.print("REDEFINES "
                            + entry.getRedefinedTarget().getName() + " ");
                }
                if (entry.getMinimumOccurs() > 1
                        && entry.getMaximumOccurs() > 1) {
                    outStream.print("OCCURS " + entry.getMinimumOccurs() + " ");
                    if (entry.getMaximumOccurs() > entry.getMinimumOccurs()) {
                        outStream.print("TO " + entry.getMaximumOccurs() + " ");
                    }
                }
                CocoDescriptionEntry occursEntry = entry.getOccursOn();
                if (occursEntry != null) {
                    outStream.print(
                            "DEPENDS ON " + occursEntry.getName() + " ");
                    List qualifiers = occursEntry.getDependOnNameQualifiers();
                    if ( qualifiers != null && qualifiers.size() > 0 ) {
                    	for ( int i = 0; i < qualifiers.size(); i++ ) {
                    		outStream.print(" OF " + qualifiers.get(i));
                    	}
                    }
                }
            }
            outStream.println();
        }
        outStream.flush();
    }
    
    /**
     * check all the base name conflicts and make them unique
     * note that name conflict in the same scope (same level number under a
     * group item) or across level number hierarchy should be name-mangled so
     * that they are all unique - otherwise, the name conflicts will cause 
     * type conflict at OTD gen time (this issue might be resolved at OTD gen
     * time but we prefer to resolve it here anyway)
     */
    public void makeBaseNamesUnique() {
    	Vector l = null;
    	int suffix_count = 0;
    	CocoDescriptionEntry entry = null;
    	Iterator it = this.mBaseNames.keySet().iterator();
    	if ( it == null )
    		return;
    	while ( it.hasNext() ) {
    		String key = (String)it.next();
    		l = (Vector)this.mBaseNames.get(key);
    		if ( l == null ) {
    			// fatal error
                Message msg = MessageCatalog.getMessage("CCCB4227");
                String text = msg.formatText(new Object[] {
                        "Resolving name conflicts after copy book parsing "
                            + "and before OTD generation.",
    					"CocoDataModel.makeBaseNamesUnique()",
    					"No entry found for registered base name : " + key
                    });
                mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                throw new IllegalArgumentException(text);
    		}
    		// mangle the base name for each entry in the list by suffixing it
    		// with an under scure and a global index - this won't conflict
            // with existing  data name since under score is not a cobol char.
    		if ( l.size() > 1 ) {
    			// the first one alway keep its name
    			// so skip l[0] and rename l[1] - l[n]
    			for ( int i = 1; i < l.size(); i++ ) {
    				entry = (CocoDescriptionEntry)l.get(i);
    				entry.setName(entry.getName() + "_" + suffix_count++);
    			}
    		}
    	}
    }
    
    /**
     * resolve all the OCCURS that has forward referencing DEPENDING ON ...
     */
    public void resolveOccurs() {
    	if ( this.mUnresolvedOccurs != null
                && this.mUnresolvedOccurs.size() > 0 ) {
    		int cnt = this.mUnresolvedOccurs.size();
    		CocoDescriptionEntry entry = null;
    		String baseName = null;
    		List qualifiers = null;
    		for ( int i = 0; i < cnt; i++ ) {
    			entry = (CocoDescriptionEntry)this.mUnresolvedOccurs.get(i);
    			qualifiers = entry.getDependOnNameQualifiers();
    			baseName = entry.getDependOnName();
    			if ( baseName != null )
    				baseName = baseName.toUpperCase();
    			String fqName = getQualifiedName(qualifiers, baseName);
    			List depends = this.lookupEntry(qualifiers, baseName, 2); 
    			if ( depends == null || depends.size() == 0 ) {
    				// dangling reference
    	            Message msg = MessageCatalog.getMessage("CCCB4226");
    	            String text = msg.formatText(new Object[] {
    	            		baseName,
    						fqName
    	            });
    	            mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
    	            throw new IllegalArgumentException(text);
    			}
    			else if ( depends.size() >= 2 ) {
    				// ambiguity - there are more than one entry that match
                    // the name
    				Message msg = MessageCatalog.getMessage("CCCB4225");
    	            String text = msg.formatText(
	            		new Object[] {
            				baseName,
    						fqName,
							((CocoDescriptionEntry) depends.get(0)).getInfo(),
							((CocoDescriptionEntry) depends.get(1)).getInfo()
            			});
    	            mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
    	            throw new IllegalArgumentException(text);
    			}
    			// now resolved
    			// further validate
    			CocoDescriptionEntry dependsObj =
                    (CocoDescriptionEntry)depends.get(0);
    			if (dependsObj.getMinimumOccurs() != 1
                        && dependsObj.getMaximumOccurs() != 1) {
    	            Message msg = MessageCatalog.getMessage("CCCB4211");
    	            String text = msg.formatText(new Object[] {
    	                    dependsObj.getName()
    	            });
    	            mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
    	            throw new IllegalArgumentException(text);
                } else if (dependsObj.getJavaType()
                            == CocoDescriptionEntry.JavaType.LONG
                        || dependsObj.getJavaType()
                            == CocoDescriptionEntry.JavaType.BIGDEC) {
    	            Message msg = MessageCatalog.getMessage("CCCB4212");
    	            String text = msg.formatText(new Object[] {
    	                    dependsObj.getName()
    	            });
    	            mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
    	            throw new IllegalArgumentException(text);
                } else if (dependsObj.getJavaType()
                        != CocoDescriptionEntry.JavaType.INT) {
    	            Message msg = MessageCatalog.getMessage("CCCB4213");
    	            String text = msg.formatText(new Object[] {
    	                    dependsObj.getName()
    	            });
    	            mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
    	            throw new IllegalArgumentException(text);
                }
    			entry.setDependsOnTarget(dependsObj);
    		}
    	}
    }
    
    /**
     * helper
     * @param qualifiers
     * @param base
     * @return
     */
    private String getQualifiedName(List qualifiers, String base) {
    	StringBuffer sb = new StringBuffer(base);
    	if ( qualifiers != null && qualifiers.size() > 0 ) {
    		for ( int i = 0; i < qualifiers.size(); i++ ) {
    			sb.append(" OF ").append(qualifiers.get(i));
    		}
    	}
    	return sb.toString();
    }
    
    /**
     * return a list of matching entries in the model
     * @param qualifiers - qualifiers for the base name
     * @param name - the base name
     * @param count - return only the first <code>count</code> number of entries 
     * @return the list of entries that its FQN match the given qualified name. 
     */
    private List lookupEntry(List qualifiers, String name, int count) {
    	List result = null;
    	String currentName = null;
    	Iterator it = this.mNames.keySet().iterator();
    	CocoDescriptionEntry parent, entry = null;
    	Vector entry_list = null;
    	int cnt = 0;
    	boolean matched = true;
    
    	while ( it.hasNext() ) {
    		currentName = (String)it.next();
    		if ( currentName.endsWith(name) ) {
    			// only when the FQN ends with the given base name
    			// we look into the entries further
    			matched = true;
    			// get the entry and check the base name
    			entry_list = (Vector)(this.mNames.get(currentName));
    			// in the entry_list, all the entry has same FQN - so check
                // one is check all
    			entry = (CocoDescriptionEntry)entry_list.get(0);
    			if ( name.equalsIgnoreCase(entry.getName()) ) {
    				// base name match
    				if ( qualifiers != null && qualifiers.size() > 0 ) {
    					// also need to match qualifiers
    					CocoDescriptionEntry tempEntry = entry; 
    					for ( int j = 0; j < qualifiers.size(); j++ ) {
    						parent = tempEntry.getParent();
    						if ( parent == null ) {
    							// path does not match the qualifiers -
                                // not a match
    							matched = false;
    							break;
    						}
    						String qualifierName = (String)qualifiers.get(j);
    						if ( qualifierName != null
                                    && parent.getName().equalsIgnoreCase(
                                            qualifierName.toUpperCase())) {
    							// still match
    							tempEntry = parent;
    						}
    						else {
    							// no longer match
    							matched = false;
    							break;
    						}
    					}
    				}
    				
    				// remember those matched
    				if ( matched ) {
    					if ( result == null )
    						result = new ArrayList();
    					result.addAll(entry_list);
    					cnt += entry_list.size();
    					if ( cnt >= count )
    						break;
    				}
    			}
    		}
    	}
    	return result;
    }
    
    /**
     * register an unresolved entry (OCCURS ... DEPENDING ON <data-name>)
     * @param entry
     */
    public void addOccurs(CocoDescriptionEntry entry) {
    	if ( this.mUnresolvedOccurs == null )
    		this.mUnresolvedOccurs = new ArrayList();
    	this.mUnresolvedOccurs.add(entry);
    }
    
    /**
     * register an entry into base name collision table 
     * - later will be used to resolve these conflicts 
     * by rename them (name mangling so that they become globally unique)
     * @param entry
     */
    private void registerBaseName(CocoDescriptionEntry entry) {
    	Vector l = null;
    	if ( this.mBaseNames.containsKey(entry.getName()) ) {
        	// register the entry with its base name as key
        	l = (Vector)this.mBaseNames.get(entry.getName());
        	if ( l == null ) {
        		// fatal error
                Message msg = MessageCatalog.getMessage("CCCB4227");
                String text = msg.formatText(new Object[] {
                        "Registering base name.",
    					"CocoDataModel.registerBaseName()",
    					"No entry found for registered base name : "
                            + entry.getName()
                    });
                mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                throw new IllegalArgumentException(text);
        	}
        	int sz = l.size();
        	l.add(sz, entry);
    	}
        else {
        	// first time this base name encountered
        	l = new Vector(1);
        	l.add(0, entry);
        	this.mBaseNames.put(entry.getName(), l);
        }
    }
    
    public void registerFQN(CocoDescriptionEntry entry) {
    	if ( this.mNames.containsKey(entry.getFQN()) ) {
    		Vector l = (Vector)mNames.get(entry.getFQN());
    		if ( l != null ) {
    			int sz = l.size();
    			l.add(sz, entry);
    		}
    		else {
    			// fatal error
                Message msg = MessageCatalog.getMessage("CCCB4227");
                String text = msg.formatText(new Object[] {
                        "Registering FQN - fully qualified name.",
    					"CocoDataModel.registerFQN()",
    					"No entry found for registered FQN : " + entry.getFQN()
                    });
                mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                throw new IllegalArgumentException(text);
    		}
    	}
    	else {
    		// first comer 
    		// assume most of time - no dup names
    		List l = new Vector(1);
    		l.add(0, entry);
    		mNames.put(entry.getFQN(), l);
    	}
    }
}
