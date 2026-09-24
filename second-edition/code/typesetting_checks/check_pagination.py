import pymupdf as fitz,re,collections,json
from PIL import Image,ImageOps,ImageDraw
root='output/bb166-pagination/'
summary={}
for trim in ['paperback','hardcover']:
 result={}
 for state in ['before','after']:
  d=fitz.open(f'{root}{state}-{trim}.pdf'); lines=collections.defaultdict(lambda:collections.Counter())
  for i,p in enumerate(d):
   for m in re.finditer(r'(R\d+I\d+N\d+)L(\d+)|LONG(\d+)',p.get_text()): lines[m[1] or 'LONG'][i+1]+=1
  bad={k:dict(v) for k,v in lines.items() if len(v)>1 and 1 in v.values()}
  assert len(lines)==37,(state,len(lines))
  for k,v in lines.items(): assert sum(v.values())==(110 if k=='LONG' else int(k.split('N')[1])),(k,v)
  markers={token:[i+1 for i,p in enumerate(d) if token in p.get_text()] for token in ['HEADINGMARK','NOTEAFTERHEADING','FOOTNOTE_ALPHA','FOOTNOTE_BETA','FOOTNOTE_GAMMA','BLOCKMARK','LISTPARA','TABLECAPTION','TABLECELL1','TABLECELL2']}
  for token,pp in markers.items(): assert len(pp)==1,(token,pp)
  for token in ['FOOTNOTE_ALPHA','FOOTNOTE_BETA','FOOTNOTE_GAMMA']:
   assert sum(p.get_text().count(token) for p in d)==1,(state,token)
  for p in d:
   if state=='after' and p.search_for('R53I1N5L1'):
    first=p.search_for('R53I1N5L1')[0]; second=p.search_for('R53I1N5L2')[0]
    assert abs(first.x0-second.x0-11.3)<.01,'first-line indent changed'
   if state=='after' and p.search_for('COLUMNMARK1'):
    positions=[p.search_for('COLUMNMARK'+str(i))[0] for i in range(1,5)]
    assert len(set(round(r.x0,1) for r in positions))==1,'column orphan persists'
  text=' '.join(p.get_text() for p in d)
  # Only normalize the deliberate chapter-source furniture introduced in BB-168.
  text=re.sub(r'\[S(\d+)\]', r'[\1]', text).replace('Sources and links', '')
  tokens=collections.Counter(re.findall(r'\w+',text))
  tokens={k:v for k,v in tokens.items() if not k.isdigit()}
  result[state]={'pages':len(d),'single_line_breaks':bad,'markers':markers,'tokens':tokens,'long_splits':dict(lines['LONG'])}
  # Render cropped page boundaries at 200 dpi for a widow and an orphan case.
  for case in ['R18I0N5','R53I1N5']:
   pages=list(lines[case]); crops=[]
   for pn in pages:
    page=d[pn-1]; pix=page.get_pixmap(dpi=200)
    im=Image.frombytes('RGB',[pix.width,pix.height],pix.samples)
    # Crop the occupied top/bottom region, retaining page edges and folio.
    if min(page.search_for(case)[0].y0,page.rect.height)>page.rect.height/2:
     im=im.crop((0,int(im.height*.78),im.width,im.height))
    else: im=im.crop((0,0,im.width,int(im.height*.30)))
    crops.append((pn,im))
   canvas=Image.new('RGB',(max(im.width for _,im in crops),sum(im.height+30 for _,im in crops)), 'white');y=0;dr=ImageDraw.Draw(canvas)
   for pn,im in crops:
    dr.text((10,y+5),f'{state} {trim} {case} PDF page {pn}',fill='black');y+=30;canvas.paste(im,(0,y));y+=im.height
   canvas.save(f'{root}{state}-{trim}-{case}.png')
 assert result['before'].pop('tokens')==result['after'].pop('tokens'),'text tokens changed'
 assert not result['after']['single_line_breaks'],result['after']['single_line_breaks']
 assert result['after']['markers']['HEADINGMARK']==result['after']['markers']['NOTEAFTERHEADING']
 summary[trim]=result
open(root+'results.json','w').write(json.dumps(summary,indent=2))
print(json.dumps(summary,indent=2))
