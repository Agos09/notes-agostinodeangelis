import * as React from "react";
import { BlogHero } from "./BlogHero";
import { DecorativeBackground } from "./DecorativeBackground";

function BlogPageHeader() {
  return (
    <div className="overflow-hidden bg-white">
      <BlogHero />
      <DecorativeBackground />
    </div>
  );
}

export default BlogPageHeader;
