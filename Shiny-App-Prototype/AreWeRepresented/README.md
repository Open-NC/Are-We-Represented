Summary
=======

Local Development
-----------------

You can use docker to run this shiny app locally and make changes:

```
docker-compose up

# then visit http://localhost:3838 in your browser
```

Deployment
----------

This app is deployed to the CodeForDurham dokku server. Deployments are made
using the following method (you will need access to the dokku server to do this
- please contact a CfD volunteer to have your app updated):

```
# Add a git remote for dokku:
git remote add dokku dokku@dokku.codefordurham.com:arewerepresented

# Deploy master:
git push dokku
```
