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
 * @(#)$Id: PatternWhiteSpaceTokenizer.java,v 1.1 2010/02/15 19:23:27 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.db.hibernate;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;
import org.apache.lucene.analysis.Token;
import org.apache.lucene.analysis.TokenStream;
import org.apache.solr.analysis.TokenizerFactory;
import org.apache.lucene.analysis.Tokenizer;
import org.apache.solr.common.SolrException;

/**
 * This class extends Lucene PatternTokenizerFactory to further seperare token by white spaces
 *  for each token recognized by the specified pattern, it is further divided by white spaces
 *  
 * @author mei
 *
 */
public class PatternWhiteSpaceTokenizer implements TokenizerFactory{
    
    public static final String PATTERN = "pattern";
    public static final String GROUP = "group";
   
    protected Map<String,String> args;
    protected Pattern pattern;
    protected int group;
    
   private static Pattern WHITESPACE = Pattern.compile("\\s+");
   /**
    * Require a configured pattern
    */
   public void init(Map<String,String> args) 
   {
     this.args = args;
     String regex = args.get( PATTERN );
     if( regex == null ) {
       throw new SolrException( SolrException.ErrorCode.SERVER_ERROR, "missing required argument: "+PATTERN );
     }
     int flags = 0; // TODO? -- read flags from config CASE_INSENSITIVE, etc
     pattern = Pattern.compile( regex, flags );
     
     group = -1;  // use 'split'
     String g = args.get( GROUP );
     if( g != null ) {
       try {
         group = Integer.parseInt( g );
       }
       catch( Exception ex ) {
         throw new SolrException( SolrException.ErrorCode.SERVER_ERROR, "invalid group argument: "+g );
       }
     }
   }

   /**
    * The arguments passed to init()
    */
   public Map<String, String> getArgs() {
     return this.args;
   }
   
   /**
    * Split the input using configured pattern
    */
   public Tokenizer create(Reader  input) {
     try {
       // Read the input into a single string
       String str = IOUtils.toString( input );
       
       Matcher matcher = pattern.matcher( str );
       List<Token> tokens = (group < 0 ) 
         ? split( matcher, str )
         : group( matcher, str, group );
         
       final Iterator<Token> iter = tokens.iterator();
       return new Tokenizer() {
         @Override
         public Token next() throws IOException {
           if( iter.hasNext() ) {
             return iter.next();
           }
           return null;
         }
       };
     }
     catch( IOException ex ) {
       throw new SolrException( SolrException.ErrorCode.SERVER_ERROR, ex );
     }
   }
   
    /**
     * This behaves just like String.split( ), but returns a list of Tokens
     * rather then an array of strings
     */
    public static List<Token> split( Matcher matcher, String input )
    {
      int index = 0;
      int lastNonEmptySize = Integer.MAX_VALUE;
      ArrayList<Token> matchList = new ArrayList<Token>();

      // Add segments before each match found
      while(matcher.find()) {
        String match = input.subSequence(index, matcher.start()).toString();
        List<Token> subTokens = getSubTokens (match, index);
        matchList.addAll(subTokens);
        index = matcher.end();
        if(match.length() > 0 ) {
          lastNonEmptySize = matchList.size();
        }
      }

      // If no match is found, return the full string
      if (index == 0) {
        List<Token> subTokens = getSubTokens (input, 0);  
        matchList.addAll(subTokens);
      }
      else { 
        String match = input.subSequence(index, input.length()).toString();
        List<Token> subTokens = getSubTokens (match, index);  
        matchList.addAll(subTokens);
         if( match.length() > 0 ) {
          lastNonEmptySize = matchList.size();
        }
      }
      
      // Don't use trailing empty strings.  This behavior matches String.split();
      if( lastNonEmptySize < matchList.size() ) {
        return matchList.subList( 0, lastNonEmptySize );
      }
      return matchList;
    }
    

    /**
     * Create tokens from the matches in a matcher 
     */
    public static List<Token> group( Matcher matcher, String input, int group )
    {
      ArrayList<Token> matchList = new ArrayList<Token>();
      boolean find = false;
      while(matcher.find()) {
          find = true;
          String match = matcher.group(group);
          List<Token> subTokens = getSubTokens (match, matcher.start(group));  
          if (subTokens.size() > 0) {
              matchList.addAll(subTokens);
          }else {
              Token t = new Token( 
                      matcher.group(group), 
                      matcher.start(group), 
                      matcher.end(group) );
                    matchList.add( t );
              matchList.add(t );
          }
      }
          if (!find) {
              List<Token> subTokens = getSubTokens (input, 0);  
              matchList.addAll(subTokens);
          }
     return matchList;
  }


    private static List<Token> getSubTokens(String subString, int begin) {
        // TODO Auto-generated method stub
        List<Token>  subTokens = new ArrayList<Token>();
        Matcher matcher = WHITESPACE.matcher(subString);
        
        int index = 0;
        int lastNonEmptySize = Integer.MAX_VALUE;
        ArrayList<Token> matchList = new ArrayList<Token>();

        // Add segments before each match found
        while(matcher.find()) {
          String match = subString.subSequence(index, matcher.start()).toString();
          matchList.add( new Token( match, begin + index, begin + match.length()) );
          index = matcher.end();
          if( match.length() > 0 ) {
            lastNonEmptySize = matchList.size();
          }
        }

        // If no match is found, return the full string
        if (index == 0) {
          matchList.add( new Token( subString, begin, begin + subString.length()) );
        }
        else { 
          String match = subString.subSequence(index, subString.length()).toString();
          matchList.add( new Token( match, begin +index, begin +subString.length()) );
          if( match.length() > 0 ) {
            lastNonEmptySize = matchList.size();
          }
        }
        
        // Don't use trailing empty strings.  This behavior matches String.split();
        if( lastNonEmptySize < matchList.size() ) {
          return matchList.subList( 0, lastNonEmptySize );
        }
        return matchList;
    }
    
    public static void main(String[] args)  throws Exception {
        
        PatternWhiteSpaceTokenizer tokenizer   = new PatternWhiteSpaceTokenizer ();
        Map<String, String> initParams = new HashMap<String, String> ();
        initParams.put("pattern", "(\\[)([^\\]]*)(\\])");
        initParams.put("group", "2");
        tokenizer.init(initParams);
        TokenStream stream = tokenizer.create(new StringReader ("[qerqwr adf][4355 asdfas][qrewqre adaf]"));
        Token tok = stream.next(); 
        while (tok != null) {
            System.out.println (tok.term());
            tok = stream.next(); 
        }
         stream = tokenizer.create(new StringReader ("34123 asdfsaa dfsadf"));
         tok = stream.next(); 
        while (tok != null) {
            System.out.println (tok.term());
            tok = stream.next(); 
        }
    }
}
    
