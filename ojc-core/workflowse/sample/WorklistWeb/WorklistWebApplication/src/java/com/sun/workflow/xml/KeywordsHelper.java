/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.workflow.xml;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *
 * @author mei
 */
public class KeywordsHelper {
    static Pattern p = Pattern.compile("(\\[)([^\\]]*)(\\])");

    public static List<String> getKeywords (String keywordsStr) {
       List<String> keywords = new ArrayList<String> ();
       Matcher m = p.matcher(keywordsStr);
        while (m.find()) {
            keywords.add(m.group(2));
        }
       return keywords;
    }


   public static void main(String[] args) {
       List<String> keywords = getKeywords ("[Keyword1][Keyword2][Keyword3]");
       for (String keyword : keywords) {
           System.out.println(keyword);
       }
   }

}
