# To obtain this design from courspage_template:
1. Remove all but {% include navbar.html %} in navbar.html and navbar.html
2. Remove `<a href="javascript:void(0)" class="closebtn" onclick="closeNav()">&times;</a>` from navbar.html
3. Only keep content between inner <div> in navbar.html
5. Replace _includes/css.html in the target repository with the corresponding css here
6. add breaks to top-box.html
7. Add tema.jpg to root and image:tema.jpg to _config.yml

## Easy way: 
1. Remove `<a href="javascript:void(0)" class="closebtn" onclick="closeNav()">&times;</a>` from navbar.html and all outside inner <div>
2. Replace navbar.html, navbar.html, css.html, top-box.html
3. Add tema.jpg to root and image:tema.jpg to _config.yml

## The changes to the css were:
1. all occurences of "sidenav"
2. css between #top-box and #fagundertittel 

# Todo:
1. Remove  navbar.html and navbar.html and include in stead just navbar.html
2. Simplify by removing start.md and in stead let user edit index.md
   
